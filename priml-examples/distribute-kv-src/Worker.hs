{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Worker where

import Control.Concurrent.STM
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import GHC.Generics (Generic)

type WorkerDB = TVar (Map Key Value)

type Key = String

type Value = String

data Message
  = Set Key Value
  | Get Key (SendPort (Maybe Value))
  deriving (Typeable, Generic, Show)

instance Binary Message

_createDB :: Process WorkerDB
_createDB = liftIO $ newTVarIO Map.empty

_set :: WorkerDB -> Key -> Value -> Process ()
_set db k v =
  liftIO $
    atomically $
      modifyTVar' db (\db' -> Map.insert k v db')

_get :: WorkerDB -> Key -> Process (Maybe Value)
_get db k =
  liftIO $
    atomically $ do
      m <- readTVar db
      return $ Map.lookup k m

worker :: Process ()
worker = do
  db <- _createDB
  forever $ do
    m <- expect
    case m of
      Set k v -> _set db k v
      Get k c -> do
        r <- _get db k
        sendChan c r

remotable ['worker]
