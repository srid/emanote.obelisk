module G.Db where

import Control.Concurrent.STM (TChan, newTChanIO, readTChan, writeTChan)
import Data.Conflict (Conflict)
import Data.Default
import G.Graph (Graph, PatchGraph)
import qualified G.Graph as Graph
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Reflex (Patch (apply), PatchMap)
import Reflex.Class (PatchMap (unPatchMap))
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
  { _zk_zettels :: Map M.WikiLinkID Zettel,
    _zk_graph :: Graph
  }

instance Default Zk where
  def = Zk mempty Graph.empty

data ZkPatch = ZkPatch
  { _zkPatch_zettels :: PatchMap M.WikiLinkID Zettel,
    _zkPatch_graph :: PatchGraph
  }

mkZkPatch :: PatchMap M.WikiLinkID (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError ([M.WikiLinkID], Pandoc))) -> ZkPatch
mkZkPatch p =
  let _zkPatch_zettels =
        (fmap . fmap . fmap . fmap) snd p
      _zkPatch_graph =
        Graph.PatchGraph . unPatchMap $
          p
            <&> concat . \case
              Right (_, Right (ids, _)) -> [ids]
              _ -> []
   in ZkPatch {..}

applyZkPatch :: ZkPatch -> Zk -> Maybe Zk
applyZkPatch ZkPatch {..} Zk {..} =
  let mzs = apply _zkPatch_zettels _zk_zettels
      mg = apply _zkPatch_graph _zk_graph
   in if isJust mzs || isJust mg
        then Just $ Zk (fromMaybe _zk_zettels mzs) (fromMaybe _zk_graph mg)
        else Nothing

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
      patch <- atomically (readTChan _db_changes)
      atomically $ do
        x <- readTVar _db_data
        case applyZkPatch patch x of
          Nothing -> pure ()
          Just x' -> writeTVar _db_data x'
      go
