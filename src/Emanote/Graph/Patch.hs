{-# LANGUAGE TypeFamilies #-}

module Emanote.Graph.Patch where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import Emanote.Graph (E, Graph (Graph), V, postSetWithLabel)
import Reflex.Patch.Class (Patch (..))

newtype PatchGraph = PatchGraph {unPatchGraph :: Map V (Maybe [(E, V)])}

instance Patch PatchGraph where
  type PatchTarget PatchGraph = Graph
  apply (PatchGraph p) (Graph graph) =
    Graph <$> patch p graph
    where
      patch ::
        (Ord v, Eq e, Monoid e) =>
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
        (Ord v, Eq e, Monoid e, MonadState (AM.AdjacencyMap e v) m) =>
        (v, Maybe [(e, v)]) ->
        m Bool
      patchVertex (v, mes) =
        case mes of
          Nothing ->
            gets (AM.hasVertex v) >>= \case
              True -> modify (AM.removeVertex v) >> pure True
              False -> pure False
          Just es -> do
            g <- get
            let esOld = toList $ postSetWithLabel v g
            if es == esOld
              then
                gets (AM.hasVertex v) >>= \case
                  True -> pure False
                  False -> modify (AM.overlay (AM.vertex v)) >> pure True
              else do
                -- Remove all edges, then add new ones back in.
                forM_ esOld $ \(_, v2) ->
                  modify $ AM.removeEdge v v2
                let newVertexOverlay =
                      AM.vertex v
                        `AM.overlay` AM.edges
                          ( (\(e, v1) -> (e, v, v1)) <$> es
                          )
                modify $
                  AM.overlay newVertexOverlay
                pure True
