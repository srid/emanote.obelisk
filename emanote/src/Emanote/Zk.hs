{-# LANGUAGE DataKinds #-}

module Emanote.Zk where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import Data.Tagged
import Emanote.Graph (Graph)
import Emanote.Graph.Patch (PatchGraph)
import qualified Emanote.Markdown.WikiLink as M
import Emanote.Zk.Type
import Reflex (PatchMap)
import Reflex.TIncremental (TIncremental)
import qualified Reflex.TIncremental as TInc
import Relude

data Zk = Zk
  { _zk_zettels :: TIncremental (PatchMap M.WikiLinkID Zettel),
    _zk_graph :: TIncremental PatchGraph,
    _zk_processStateRev :: TVar Rev
  }

type Rev = Tagged "Rev" Integer

run :: Zk -> IO ()
run Zk {..} = do
  void $ forkIO $ TInc.runTIncremental increaseRev _zk_zettels
  TInc.runTIncremental increaseRev _zk_graph
  where
    increaseRev = do
      STM.modifyTVar' _zk_processStateRev (+ 1)

getZettels :: MonadIO m => Zk -> m (Map M.WikiLinkID Zettel)
getZettels =
  TInc.readValue . _zk_zettels

getGraph :: MonadIO m => Zk -> m Graph
getGraph =
  TInc.readValue . _zk_graph

getRev :: MonadIO m => Zk -> m Rev
getRev =
  readTVarIO . _zk_processStateRev