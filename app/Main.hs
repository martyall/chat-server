module Main
  (main) where

import GHC.IO.Handle          (hClose)
import Control.Monad          (forever)
import Network                (listenOn, accept, PortID(..))
import Control.Concurrent     (forkFinally)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO)
import Text.Printf            (printf)
import Lib

main :: IO ()
main = do
  sock <- listenOn (PortNumber (fromIntegral port))
  server <- mkServer
  userCount <- newTVarIO (0 :: ClientId)
  printf "Listening on port %d\n" port
  forever $ do
    (handle, host, port) <- accept sock
    client <- atomically $ do
      n <- readTVar userCount
      let c = mkClient n handle
      cs <- readTVar $ getClients server
      writeTVar (getClients server) (c : cs)
      writeTVar userCount (succ n)
      return c
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (runClient server client) (\_ -> hClose handle)

port :: Int
port = 44444
