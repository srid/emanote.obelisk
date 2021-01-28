{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module G.Db where

import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan)
import Data.Conflict (Conflict)
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Reflex
import Text.Pandoc.Definition

data Db = Db
  { _db_data :: TVar (Map M.ID (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc))),
    _db_changes :: TChan (PatchMap M.ID (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc)))
  }

newDb :: IO Db
newDb =
  Db <$> newTVarIO mempty <*> newTChanIO

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
        case apply patch x of
          Nothing -> pure ()
          Just x' -> writeTVar _db_data x'
      go
