module Lib where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Monoid
import GHC.IO.Handle
import System.IO

--------------------------------------------------------------------------------
-- | Client
--------------------------------------------------------------------------------

data Client = Client { clientUsername :: String
                     , clientHandle   :: Handle
                     , messageChannel :: TChan Message
                     }

mkClient :: Int -> Handle -> STM Client
mkClient n h = do
  chan <- newTChan
  let username = "user" ++ show n
  return $ Client username h chan

runClient :: Server -> Client -> IO ()
runClient server client = do
    race publishToChannel publishToServer
    return ()
  where
    publishToChannel :: IO ()
    publishToChannel = forever $ do
      msg <- hGetLine $ clientHandle client
      atomically $ sendMessage client (ToPublish msg)
    publishToServer :: IO ()
    publishToServer = do
      join $ atomically $ do
        msg <- readTChan $ messageChannel client
        return $ do
          handleMessage server client msg
          publishToServer

handleMessage :: Server -> Client -> Message -> IO ()
handleMessage server client msg = case msg of
  ToPublish m -> let m' = clientUsername client <> ": " <> m
                 in atomically $ broadcastMessage server (ToBroadcast m')
  ToBroadcast m -> hPutStrLn (clientHandle client) m

--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

data Server = Server { getClients :: TVar [Client] }

mkServer :: IO Server
mkServer = do
  cs <- newTVarIO []
  return $ Server cs

--------------------------------------------------------------------------------
-- | Message
--------------------------------------------------------------------------------

data Message = ToPublish String
             | ToBroadcast String
               deriving (Eq, Show, Read)

broadcastMessage :: Server -> Message -> STM ()
broadcastMessage server msg = do
    clients <- readTVar $ getClients server
    mapM_ (\client -> sendMessage client msg) clients

sendMessage :: Client -> Message -> STM ()
sendMessage cl msg = writeTChan (messageChannel cl) msg


someFunc :: IO ()
someFunc = putStrLn "someFunc"
