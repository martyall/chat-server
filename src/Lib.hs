{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (ClientId(..)
           ,Client(..)
           ,mkClient
           ,runClient
           ,Server(..)
           ,mkServer
           ,joinRoom
           ,removeClient
           ) where

import           Control.Monad                (forever, join, void)
import           Control.Concurrent.Async     (race)
import           Control.Concurrent.STM       (TVar, readTVar, newTVarIO, writeTVar
                                              ,TChan, newTChan, readTChan, writeTChan
                                              ,STM, atomically)
import           Data.Char                    (isSpace)
import           Data.List                    (delete)
import qualified Data.Map as M
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           GHC.IO.Handle                (Handle, hClose)
import           System.IO                    (hGetLine, hPutStrLn)
import           Text.ParserCombinators.ReadP


--------------------------------------------------------------------------------
-- | Client
--------------------------------------------------------------------------------

newtype ClientId = ClientId { getClientId :: Int } deriving (Eq, Enum, Num)

data Client = Client { clientId           :: ClientId
                     , clientHandle       :: Handle
                     , currentRoom        :: Room
                     , clientMessageQueue :: TChan Message
                     } deriving Eq

mkClient :: ClientId -> Handle -> STM Client
mkClient n h = do
  ch <- newTChan
  return $ Client n h "general" ch

joinRoom :: Server -> Client -> Room -> STM ()
joinRoom (Server channelsVar) client room = do
  channels <- readTVar $ channelsVar
  case M.lookup room channels of
    Nothing -> writeTVar channelsVar $ M.insert room [client] channels
    Just cs -> do
      broadcastMessage' cs (Entered $ clientId client)
      writeTVar channelsVar $ M.insert room (client : cs) channels

exitRoom :: Server -> Client -> STM ()
exitRoom (Server channelsVar) client = do
  let room = currentRoom client
  channels <- readTVar $ channelsVar
  case M.lookup room channels of
    Nothing -> return ()
    Just [client] -> writeTVar channelsVar $ M.delete room channels
    Just cs -> do
      let cs' = delete client cs
      writeTVar channelsVar $ M.insert room cs' channels
      broadcastMessage' cs' (Exited $ clientId client)

changeRooms :: Server -> Client -> Room -> STM ()
changeRooms server client newRoom = do
  exitRoom server client
  joinRoom server client newRoom

runClient :: Server -> Client -> IO ()
runClient server client = do
    race publish receive
    return ()
  where
    handle :: Handle
    handle = clientHandle client
    publish :: IO ()
    publish = forever $ do
      msg <- parseMessage <$> hGetLine handle
      atomically $ handleMessage server client msg
    receive :: IO ()
    receive = do
      let msgQueue = clientMessageQueue client
      msg <- atomically $ readTChan msgQueue
      formatMessage msg >>= hPutStrLn handle
      receive

--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

type Room = String

data Server = Server { getChannelMap :: TVar (M.Map String [Client]) }

mkServer :: IO Server
mkServer = do
  cs <- newTVarIO $ M.fromList [("general", [])]
  return $ Server cs

removeClient :: Server -> Client -> IO ()
removeClient server client = do
  atomically $ exitRoom server client
  hClose $ clientHandle client

--------------------------------------------------------------------------------
-- | Message
--------------------------------------------------------------------------------

data Message = Msg String
             | Join String
             | PM ClientId String
             | Error String
             | Entered ClientId
             | Exited ClientId

handleMessage :: Server -> Client -> Message -> STM ()
handleMessage server client msg = case msg of
  m@(Msg _) -> broadcastMessage server client m
  Join room -> changeRooms server client room
  PM cId msg -> error "implement pm"
  err@(Error _) -> sendMessage client err
  _ -> return ()

broadcastMessage :: Server -> Client -> Message -> STM ()
broadcastMessage (Server channelsVar) client msg = do
  channels <- readTVar channelsVar
  let room = currentRoom client
  fromMaybe (return ()) $ do
    cs <- M.lookup room channels
    return $ broadcastMessage' cs msg

broadcastMessage' :: [Client] -> Message -> STM ()
broadcastMessage' clients msg = mapM_ (\client -> sendMessage client msg) clients

sendMessage :: Client -> Message -> STM ()
sendMessage client msg =
  let msgQueue = clientMessageQueue client
  in writeTChan msgQueue msg

formatMessage :: Message -> IO String
formatMessage = undefined

parseMessage :: String -> Message
parseMessage s = case s of
  cmd@('/':rest) -> case readP_to_S (parseJoin +++ parsePM) cmd of
    [] -> Error $ "unknown command: " <> cmd
    c:_ -> fst c
  s' -> Msg s'

parseJoin :: ReadP Message
parseJoin = do
  string "/join"
  skipSpaces
  room <- munch1 $  not . isSpace
  eof
  return $ Join room

parsePM :: ReadP Message
parsePM = do
  string "/msg"
  skipSpaces
  cid <- munch1 $ not . isSpace
  msg <- manyTill get eof
  return $ PM (ClientId $ read cid) msg
