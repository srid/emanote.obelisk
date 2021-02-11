{-# LANGUAGE TypeFamilies #-}

module Emanote.Graph.Patch where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Emanote.Graph (E, Graph (Graph), V, postSetWithLabel)
import Reflex.Patch.Class (Patch (..))
import Relude

-- | NOTE: Patching a graph may leave orphan vertices behind. Use
-- @patchedGraphVertexSet@ to get the effective list of vertices.
newtype PatchGraph = PatchGraph {unPatchGraph :: Map V (Maybe [(E, V)])}

instance Patch PatchGraph where
  type PatchTarget PatchGraph = Graph
  apply (PatchGraph p) (Graph graph) =
    Graph <$> patch p graph
    where
      patch ::
        (Ord v, Show v, Show e, Eq e, Monoid e) =>
        Map v (Maybe [(e, v)]) ->
        AM.AdjacencyMap e v ->
        Maybe (AM.AdjacencyMap e v)
      patch diff g =
        let (changed, g') = flip runState g $ do
              patchVertex `mapM` Map.toList diff
         in do
              guard $ or changed
              pure g'
      patchVertex ::
        (Ord v, Show v, Show e, Eq e, Monoid e, MonadState (AM.AdjacencyMap e v) m) =>
        (v, Maybe [(e, v)]) ->
        m Bool
      patchVertex (v, mes) =
        case mes of
          Nothing -> do
            g <- get
            gets (AM.hasVertex v) >>= \case
              True -> do
                if Set.null $ AM.preSet v g
                  then -- Delete only if no other vertex is connecting to us.
                    modify (AM.removeVertex v) >> pure True
                  else pure False
              False ->
                pure False
          Just es -> do
            g <- get
            let esOld = toList $ postSetWithLabel v g
            if es == esOld
              then
                if null es
                  then
                    gets (AM.hasVertex v) >>= \case
                      -- No edges, so we must manually add the orphan vertex
                      False -> modify (AM.overlay (AM.vertex v)) >> pure True
                      True -> pure False
                  else pure False
              else do
                -- Remove all edges, then add new ones back in.
                forM_ esOld $ \(_, v2) ->
                  modify $ AM.removeEdge v v2
                let newVertexOverlay =
                      AM.edges
                        ( (\(e, v1) -> (e, v, v1)) <$> es
                        )
                modify $
                  AM.overlay newVertexOverlay
                -- NOTE: if a v2 got removed, and it is not linked in other
                -- vertices, we should remove it *IF* there is not actual note
                -- on disk. But this "actula note" check is better decoupled,
                -- and checked elsewhere. See patchedGraphVertexSet below.
                pure True

-- | Return the vertices in the graph with the given pruning function.
--
-- Use this function to accomodate for PatchGraph's idiosyncratic behaviour of
-- leaving orphans behind.
--
-- Prunes only orphan vertices. i.e., the pruning function cannot prune vertices
-- with non-zero edges.
patchedGraphVertexSet :: (V -> Bool) -> AM.AdjacencyMap E V -> Set V
patchedGraphVertexSet exists g =
  let nonOrphans = connectedVertices g
      orphans = AM.vertexSet g `Set.difference` nonOrphans
      orphansExisting = Set.filter exists orphans
   in nonOrphans <> orphansExisting
  where
    -- Because patching a graph can leave orphan vertices behind (which may or may
    -- not correspond to actual thing), this function maybe used *in conjunction
    -- with* the store of actual things, to determined the effective list of
    -- vertices.
    connectedVertices :: AM.AdjacencyMap E V -> Set V
    connectedVertices =
      Set.fromList . concatMap (\(_e, v1, v2) -> [v1, v2]) . AM.edgeList
