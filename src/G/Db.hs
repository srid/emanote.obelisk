{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module G.Db where

import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan)
import Data.Conflict (Conflict)
import Data.Default
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Reflex (Patch (apply), PatchMap)
import Text.Pandoc.Definition (Pandoc)

type Zettel =
  Either
    -- More than one file uses the same ID.
    (Conflict FilePath ByteString)
    ( -- Path on disk
      FilePath,
      Either
        -- Failed to parse markdown
        M.ParserError
        -- Pandoc AST
        Pandoc
    )

data Zk = Zk
  { _zk_zettels :: Map M.ID Zettel,
    _zk_graph :: ()
  }

instance Default Zk where
  def = Zk mempty ()

-- | A dumb database to manage Haskell values in-memory across threads, while
-- supporting a way to "patch" them.
data Db = Db
  { -- | Snapshot of the data at any point in time
    _db_data :: TVar Zk,
    -- | The channel to stream reflex patches from Incremental
    _db_changes :: TChan (PatchMap M.ID Zettel)
  }

newDb :: IO Db
newDb =
  Db <$> newTVarIO def <*> newTChanIO

push :: Db -> PatchMap M.ID (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc)) -> IO ()
push Db {..} =
  atomically . writeTChan _db_changes

run :: Db -> IO ()
run Db {..} =
  go
  where
    go = do
      patch <- atomically (readTChan _db_changes)
      atomically $ do
        x <- readTVar _db_data
        case apply patch (_zk_zettels x) of
          Nothing -> pure ()
          Just zs -> writeTVar _db_data $ x {_zk_zettels = zs}
      go
