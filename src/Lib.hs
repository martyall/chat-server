{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (ClientId(..)
           ,Client(..)
           ,mkClient
           ,runClient
           ,Server(..)
           ,mkServer
           ,addToRoom
           ,addClient
           ,removeClient
           ) where

import           Control.Monad                (forever, join, void)
import           Control.Concurrent.Async     (race)
import           Control.Concurrent.STM       (TVar, readTVar, newTVarIO, writeTVar, newTVar
                                              ,TChan, newTChan, readTChan, writeTChan
                                              ,modifyTVar, STM, atomically)
import           Data.Char                    (isSpace)
import           Data.Foldable                (find)
import           Data.List                    (delete)
import qualified Data.Map as M
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.String                  (IsString(..))
import           GHC.IO.Handle                (Handle, hClose)
import           System.IO                    (hGetLine, hPutStrLn)
import           Text.ParserCombinators.ReadP


--------------------------------------------------------------------------------
-- | Client
--------------------------------------------------------------------------------

newtype ClientId = ClientId { getClientId :: Int } deriving (Eq, Enum, Num, Show)

data Client = Client { clientId           :: ClientId
                     , clientHandle       :: Handle
                     , currentRoom        :: TVar Room
                     , clientMessageQueue :: TChan Message
                     } deriving Eq

mkClient :: ClientId -> Room -> Handle -> STM Client
mkClient n r h = do
  ch <- newTChan
  rm <- newTVar r
  return $ Client n h rm ch

addClient :: Server -> Client -> STM ()
addClient server client = do
  room <- readTVar $ currentRoom client
  addToRoom server client room
  modifyTVar (getClients server) (client :)

-- | Change the client's room and broadcast a "join" message to those already in the room.
addToRoom :: Server -> Client -> Room -> STM ()
addToRoom server client room = do
  let channelsVar = getChannelMap server
  writeTVar (currentRoom client) room
  channels <- readTVar $ channelsVar
  case M.lookup room channels of
    Nothing -> writeTVar channelsVar $ M.insert room [client] channels
    Just cs -> do
      broadcastMessage' cs (Entered $ clientId client)
      writeTVar channelsVar $ M.insert room (client : cs) channels

-- | Change the clients room and broadcast a "left" message to anyone remaining.
removeFromRoom :: Server -> Client -> STM ()
removeFromRoom server client = do
  let channelsVar = getChannelMap server
  room <- readTVar $ currentRoom client
  channels <- readTVar channelsVar
  case M.lookup room channels of
    Nothing -> return ()
    Just [client] -> writeTVar channelsVar $ M.delete room channels
    Just cs -> do
      let cs' = delete client cs
      writeTVar channelsVar $ M.insert room cs' channels
      broadcastMessage' cs' (Exited $ clientId client)

changeRooms :: Server -> Client -> Room -> STM ()
changeRooms server client newRoom = do
  removeFromRoom server client
  addToRoom server client newRoom

-- | runClient listens two to computations, a publisher and receiver. The
-- publisher is listening to the client's buffer and attempting to publish messages. The
-- receiver is listening to the client's message queue and displays any messages that
-- come through. Whenever one process wins out, the race starts over again.
runClient :: Server -> Client -> IO ()
runClient server client = void $ race receive publish
  where
    handle :: Handle
    handle = clientHandle client
    publish :: IO ()
    publish = forever $ do
      msg <- parseMessage <$> hGetLine handle
      atomically $ handleMessage server client msg
    receive :: IO ()
    receive = join . atomically $ do
      let msgQueue = clientMessageQueue client
      msg <- readTChan msgQueue
      return $ do
        hPutStrLn handle $ formatMessage msg
        receive

--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

newtype Room = Room String deriving (Show, Eq, Ord, IsString)

data Server = Server { getChannelMap :: TVar (M.Map Room [Client])
                     , getClients :: TVar [Client]
                     }

mkServer :: IO Server
mkServer = do
  m <- newTVarIO $ M.fromList [("general", [])]
  cls <- newTVarIO []
  return $ Server m cls

getClientById :: Server -> ClientId -> STM (Maybe Client)
getClientById server cId = do
  clients <- readTVar $ getClients server
  return $ find ((== cId) . clientId) clients

removeClient :: Server -> Client -> IO ()
removeClient server client = do
  atomically $ removeFromRoom server client
  hClose $ clientHandle client

--------------------------------------------------------------------------------
-- | Message
--------------------------------------------------------------------------------

data Command = Join Room
             | PM ClientId String

data Msg = Msg String
         | Cmd Command
         | Err String

data Message = Message String
             | Entered ClientId
             | Exited ClientId
             | Error String

-- | handleMessage takes coerces a Msg to a Message and pushes
-- it to the appropriate receiver's message queue
handleMessage :: Server -> Client -> Msg -> STM ()
handleMessage server sender msg = case msg of
    Msg m -> broadcastMessage server sender (Message $ prep m)
    (Cmd (Join room)) -> changeRooms server sender room
    (Cmd (PM cId msg)) -> do
      receiver <- getClientById server cId
      case receiver of
        Nothing -> sendMessage sender (Error $ "user " <> (show $ getClientId cId) <> "not found")
        Just r -> sendMessage r (Message $ prep msg)
    Err err -> sendMessage sender (Error err)
  where
    prep :: String -> String
    prep m = (show . getClientId . clientId $ sender) <> ": " <> m

-- | broadcaseMessage pushes a Message to the message queue of
-- every other client in the same room, except the sender himself.
broadcastMessage :: Server -> Client -> Message -> STM ()
broadcastMessage server sender msg = do
  let channelsVar = getChannelMap server
  room <- readTVar $ currentRoom sender
  channels <- readTVar channelsVar
  fromMaybe (return ()) $ do
    cs <- M.lookup room channels
    let cs' = delete sender cs
    return $ broadcastMessage' cs' msg

-- | a version of broadcastMessage for when the list of receivers is already known.
broadcastMessage' :: [Client] -> Message -> STM ()
broadcastMessage' receivers msg = mapM_ (\client -> sendMessage client msg) receivers

-- | send a Message to a single client's queue.
sendMessage :: Client -> Message -> STM ()
sendMessage receiver msg =
  let msgQueue = clientMessageQueue receiver
  in writeTChan msgQueue msg

-- | Message formatters and parsers

formatMessage :: Message -> String
formatMessage msg = case msg of
  Message m -> m
  Entered cId -> (show $ getClientId cId) <> " has entered."
  Exited cId -> (show $ getClientId cId) <> " has left."
  Error err -> err

parseMessage :: String -> Msg
parseMessage s = case s of
  cmd@('/':rest) -> case readP_to_S (parseJoin +++ parsePM) cmd of
    [] -> Err $ "unknown command: " <> cmd
    c:_ -> Cmd $ fst c
  s' -> Msg s'

parseJoin :: ReadP Command
parseJoin = do
  string "/join"
  skipSpaces
  room <- munch1 $  not . isSpace
  eof
  return $ Join (fromString room)

-- This currently is flawed-- there is no error handling and it will crash the program
parsePM :: ReadP Command
parsePM = do
  string "/msg"
  skipSpaces
  cid <- munch1 $ not . isSpace
  msg <- manyTill get eof
  return $ PM (ClientId $ read cid) msg
