{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (ClientId(..)
           ,Client(..)
           ,mkClient
           ,runClient
           ,Server(..)
           ,mkServer
           ,announceEntrance
           ,removeClient) where

import Control.Monad            (forever, join)
import Control.Concurrent.STM   (TVar, readTVar,newTVarIO, atomically, writeTVar)
import Data.List                (delete)
import Data.Monoid              ((<>))
import GHC.IO.Handle            (Handle, hClose)
import System.IO                (hGetLine, hPutStrLn)

--------------------------------------------------------------------------------
-- | Client
--------------------------------------------------------------------------------

newtype ClientId = ClientId { getClientId :: Int } deriving (Eq, Enum, Num)

data Client = Client { clientId       :: ClientId
                     , clientHandle   :: Handle
                     } deriving Eq

mkClient :: ClientId -> Handle -> Client
mkClient n h = Client n h

runClient :: Server -> Client -> IO ()
runClient server client = forever $ do
    msg <- hGetLine $ clientHandle client
    broadcastMessage server (Just $ clientId client) (Msg msg)

--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

data Server = Server { getClients  :: TVar [Client] }

mkServer :: IO Server
mkServer = do
  cs <- newTVarIO []
  return $ Server cs

announceEntrance :: Server -> Client -> IO ()
announceEntrance server client =
  broadcastMessage server Nothing (Entered $ clientId client)

removeClient :: Server -> Client -> IO ()
removeClient server client = join . atomically $ do
  let clientsVar = getClients server
  cs <- readTVar clientsVar
  writeTVar clientsVar $ delete client cs
  return $ do
    hClose $ clientHandle client
    broadcastMessage server Nothing (Exited $ clientId client)

--------------------------------------------------------------------------------
-- | Message
--------------------------------------------------------------------------------

data Message = Msg String | Entered ClientId | Exited ClientId

broadcastMessage :: Server -> Maybe ClientId -> Message -> IO ()
broadcastMessage server msenderId msg = join . atomically $ do
    clients <- readTVar $ getClients server
    let receivers = getReceivers msenderId clients
    return $ mapM_ (sendMessage msg) $ receivers
  where
    getReceivers :: Maybe ClientId -> [Client] -> [Client]
    getReceivers msId cls = case msId of
      Nothing -> cls
      Just sId -> filter ((/= sId) . clientId) cls

sendMessage :: Message -> Client -> IO ()
sendMessage msg cl = hPutStrLn (clientHandle cl) msg'
  where
    msg' = case msg of
      Msg m -> (show . getClientId . clientId $ cl) <> ": " <> m
      Entered n -> (show . getClientId $ n) <> " has entered."
      Exited n -> (show . getClientId $ n) <> " has left."
