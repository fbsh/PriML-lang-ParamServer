{-# LANGUAGE TemplateHaskell, CPP #-}
module DistribUtils ( distribMain ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)

import System.Environment

import Network.Socket hiding (shutdown)

import Language.Haskell.TH

distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let rtable = frtable initRemoteTable

  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "leader" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "leader", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startMaster backend master
    [ "follower" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startSlave backend
    [ "follower", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startSlave backend
    [ "follower", host, port ] -> do
      backend <- initializeBackend host port rtable
      startSlave backend

defaultHost = "localhost"
defaultPort = "44444"
