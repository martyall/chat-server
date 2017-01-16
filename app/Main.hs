module Main where

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import GHC.IO.Handle
import Network
import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf
import Lib

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  server <- mkServer
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    client <- atomically $ do
      c <- mkClient 0 handle
      cs <- readTVar $ getClients server
      writeTVar (getClients server) (c : cs)
      return c
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (runClient server client) (\_ -> hClose handle)

connectClients :: Server -> Socket -> StateT Int IO ()
connectClients server sock = do
   (handle, host, port) <- liftIO $ accept sock
   n <- get
   client <- liftIO $ atomically $ mkClient n handle
   liftIO $ atomically $ do
    clients <- readTVar $ getClients server
    writeTVar (getClients server) (client : clients)
   put $ n + 1
   liftIO $ printf "Accepted connection from %s: %s\n" host (show port)
   liftIO $ forkFinally (runClient server client) (\_ -> hClose handle)
   return ()

port :: Int
port = 44444
