{-# LANGUAGE TypeFamilies #-}

module Emanote.Db where

import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan)
import Data.Default (Default (..))
import Reflex (Patch (PatchTarget))
import Reflex.Patch.Class (Patch (apply))

-- | A dumb database to manage Haskell values in-memory across threads, while
-- supporting a way to "patch" them.
data Db patch = Db
  { -- | Snapshot of the data at any point in time
    _db_data :: TVar (PatchTarget patch),
    -- | The channel to stream reflex patches from Incremental
    _db_changes :: TChan patch
  }

newDb :: Default (PatchTarget patch) => IO (Db patch)
newDb =
  Db <$> newTVarIO def <*> newTChanIO

patchDb :: Db patch -> patch -> IO ()
patchDb Db {..} =
  atomically . writeTChan _db_changes

run :: Patch patch => Db patch -> IO ()
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
