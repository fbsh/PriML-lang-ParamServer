{-# LANGUAGE TemplateHaskell #-}

module Database
  ( Database,
    createDB,
    get,
    set,
    rcdata,
  )
where

import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Worker

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB peers = spawnLocal (server peers)

set :: Database -> Key -> Value -> Process ()
set pid k v = send pid (Set k v)

get :: Database -> Key -> Process (Maybe Value)
get pid k = do
  (sendPort, recvPort) <- newChan
  send pid (Get k sendPort)
  receiveChan recvPort

rcdata :: RemoteTable -> RemoteTable
rcdata = Worker.__remoteTable

server :: [NodeId] -> Process ()
server peers = do
  self <- getSelfPid
  say $ printf "spawning on %s" (show self)

  ps <- forM peers $ \nid -> do
    say $ printf "spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'worker)

  mapM_ monitor ps

  let pss = mkPairs ps
  say $ printf "processes: %s" (show pss)
  loop pss
 where
  loop pss = receiveWait
    [ match $ \m -> case m of
        s@(Set k v) -> do
          let pids = selectWorkers pss k
          mapM_ (\pid -> send pid s) pids
          loop pss
        g@(Get k _) -> do
          let pids = selectWorkers pss k
          mapM_ (\pid -> send pid g) pids
          loop pss
    , match $ \(ProcessMonitorNotification _ref deadpid reason) -> do
        say $ printf "process %s died: %s" (show deadpid) (show reason)
        let pss' = removeWorker pss deadpid
        say $ printf "processes: %s" (show pss')
        loop pss'
    ]

selectWorkers :: [[ProcessId]] -> Key -> [ProcessId]
selectWorkers pss [] = error "empty key"
selectWorkers pss (k : ey) = pss !! (ord k `mod` (length pss))

mkPairs :: [ProcessId] -> [[ProcessId]]
mkPairs [] = [[]]
mkPairs [a] = [[a]]
mkPairs (a:b:[]) = [[a, b]]
mkPairs (a:b:xs) = [a, b] : mkPairs xs

removeWorker :: [[ProcessId]] -> ProcessId -> [[ProcessId]]
removeWorker pss deadpid = filter (not . null) $ map (\ps -> filter (/=deadpid) ps) pss
