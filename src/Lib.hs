{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (ClientId(..)
           ,Client(..)
           ,mkClient
           ,runClient
           ,Server(..)
           ,mkServer) where

import Control.Monad            (forever, join)
import Control.Concurrent.STM   (TVar, readTVar,newTVarIO, atomically)
import Data.Monoid              ((<>))
import GHC.IO.Handle            (Handle)
import System.IO                (hGetLine, hPutStrLn)

--------------------------------------------------------------------------------
-- | Client
--------------------------------------------------------------------------------

newtype ClientId = ClientId { getClientId :: Int } deriving (Eq, Enum, Num)

data Client = Client { clientId       :: ClientId
                     , clientHandle   :: Handle
                     }

mkClient :: ClientId -> Handle -> Client
mkClient n h = Client n h

runClient :: Server -> Client -> IO ()
runClient server client = forever $ do
    msg <- hGetLine $ clientHandle client
    broadcastMessage server (Just $ clientId client) msg

--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

data Server = Server { getClients  :: TVar [Client] }

mkServer :: IO Server
mkServer = do
  cs <- newTVarIO []
  return $ Server cs

--------------------------------------------------------------------------------
-- | Message
--------------------------------------------------------------------------------

type Message = String

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
    msg' = (show . getClientId . clientId $ cl) <> ": " <> msg
