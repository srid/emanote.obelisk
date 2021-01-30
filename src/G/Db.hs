{-# LANGUAGE TypeFamilies #-}

module G.Db where

import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan)
import Data.Default (Default (..))
import G.Db.Types.Zk (Zk)
import G.Db.Types.Zk.Patch (ZkPatch)
import Reflex.Patch.Class (Patch (apply))

-- | A dumb database to manage Haskell values in-memory across threads, while
-- supporting a way to "patch" them.
data Db = Db
  { -- | Snapshot of the data at any point in time
    _db_data :: TVar Zk,
    -- | The channel to stream reflex patches from Incremental
    _db_changes :: TChan ZkPatch
  }

newDb :: IO Db
newDb =
  Db <$> newTVarIO def <*> newTChanIO

patchDb :: Db -> ZkPatch -> IO ()
patchDb Db {..} =
  atomically . writeTChan _db_changes

run :: Db -> IO ()
run Db {..} =
  go
  where
    go = do
      zkPatch <- atomically (readTChan _db_changes)
      atomically $ do
        x <- readTVar _db_data
        case apply zkPatch x of
          Nothing -> pure ()
          Just x' -> writeTVar _db_data x'
      go
