module Emanote.Zk where

import Control.Concurrent (forkIO)
import Emanote.Graph.Patch (PatchGraph)
import qualified Emanote.Markdown.WikiLink as M
import Emanote.Zk.Type
import Reflex (PatchMap)
import Reflex.TIncremental (TIncremental)
import qualified Reflex.TIncremental as TInc
import Relude

data Zk = Zk
  { _zk_zettels :: TIncremental (PatchMap M.WikiLinkID Zettel),
    _zk_graph :: TIncremental PatchGraph
  }

run :: Zk -> IO ()
run Zk {..} = do
  void $ forkIO $ TInc.runTIncremental _zk_zettels
  TInc.runTIncremental _zk_graph
