{-# LANGUAGE TypeFamilies #-}

module Algebra.Graph.Labelled.AdjacencyMap.Patch
  ( PatchGraph (..),
    ModifyGraph (..),
    modifiedOrAddedVertices,
    removedVertices,
    asPatchGraph,
    patchedGraphVertexSet,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Data.Patch.Class (Patch (..))
import qualified Data.Set as Set
import Relude

-- | An action that modifies the graph.
data ModifyGraph e v
  = -- | Replace (or add) a vertex along with its sucessor edges
    ModifyGraph_ReplaceVertexWithSuccessors v [(e, v)]
  | -- | Add a single edge
    ModifyGraph_AddEdge e v v
  | -- | Remove a vertex and its successors.
    --
    -- The vertex itself will be retained only if it is referenced elsewhere (ie. it is
    -- a successor of some other vertex).
    ModifyGraph_RemoveVertexWithSuccessors v
  deriving (Eq)

-- | NOTE: Patching a graph may leave orphan vertices behind. Use
-- @patchedGraphVertexSet@ to get the effective list of vertices.
newtype PatchGraph e v = PatchGraph {unPatchGraph :: [ModifyGraph e v]}

-- | Concatenation order determines the order the actions will be applied.
instance Semigroup (PatchGraph e v) where
  PatchGraph as1 <> PatchGraph as2 = PatchGraph (as1 <> as2)

instance Monoid (PatchGraph e v) where
  mempty = PatchGraph mempty

-- | Get all vertices that are potentially touched in this patch
--
-- Note that even if only a single edge is being added, we return the involved
-- vertices, as those vertices may not already exist in the patch target (full
-- graph).
modifiedOrAddedVertices :: Ord v => PatchGraph e v -> Set v
modifiedOrAddedVertices (PatchGraph actions) =
  Set.fromList $
    concat $
      actions <&> \case
        ModifyGraph_ReplaceVertexWithSuccessors v (fmap snd -> vs) ->
          v : vs
        ModifyGraph_AddEdge _ v1 v2 ->
          [v1, v2]
        ModifyGraph_RemoveVertexWithSuccessors _ ->
          []

removedVertices :: Ord v => PatchGraph e v -> Set v
removedVertices (PatchGraph actions) =
  Set.fromList $
    concat $
      actions <&> \case
        ModifyGraph_ReplaceVertexWithSuccessors {} ->
          []
        ModifyGraph_AddEdge {} ->
          []
        ModifyGraph_RemoveVertexWithSuccessors v ->
          [v]

instance (Ord v, Eq e, Monoid e) => Patch (PatchGraph e v) where
  type PatchTarget (PatchGraph e v) = AM.AdjacencyMap e v
  apply (PatchGraph modifications) graph0 =
    let (changed, g') = flip runState graph0 $ do
          modifyGraph `mapM` modifications
     in do
          guard $ or changed
          pure g'
    where
      modifyGraph ::
        (Ord v, Eq e, Monoid e, MonadState (AM.AdjacencyMap e v) m) =>
        ModifyGraph e v ->
        m Bool
      modifyGraph = \case
        ModifyGraph_RemoveVertexWithSuccessors v -> do
          gets (AM.hasVertex v) >>= \case
            True -> do
              -- First remove all successors
              succs <- gets (toList . AM.postSet v)
              forM_ succs $ \v2 ->
                modify $ AM.removeEdge v v2
              -- Then remove the vertex itself, but only if it is not being
              -- referenced from elsewhere.
              gets (Set.null . AM.preSet v) >>= \case
                True -> modify (AM.removeVertex v) >> pure True
                False -> pure $ not (null succs)
            False ->
              pure False
        ModifyGraph_AddEdge e v v2 -> do
          modify $ AM.overlay (AM.edge e v v2)
          pure True
        ModifyGraph_ReplaceVertexWithSuccessors v es -> do
          esOldWithLabels <- gets (toList . postSetWithLabel v)
          let esOld = fmap snd esOldWithLabels
          if es == esOldWithLabels
            then -- Vertex itself changed, with its connections intact

              if null es
                then
                  gets (AM.hasVertex v) >>= \case
                    -- No edges, so we must manually add the orphan vertex
                    False -> modify (AM.overlay (AM.vertex v)) >> pure True
                    True -> pure False
                else pure False
            else -- Vertex changed, along with its connections
            do
              -- Remove all edges, then add new ones back in.
              forM_ esOld $ \v2 -> do
                modify $ AM.removeEdge v v2
              let newVertexOverlay =
                    AM.edges
                      ( (\(e, v1) -> (e, v, v1)) <$> es
                      )
              modify $
                AM.overlay newVertexOverlay
              -- NOTE: if a v2 got removed, and it is not linked in other
              -- vertices, we should remove it *IF* there is no actual note
              -- on disk. But this "actual note" check is better decoupled,
              -- and checked elsewhere. See patchedGraphVertexSet below.
              pure True
      postSetWithLabel :: (Ord a, Monoid e) => a -> AM.AdjacencyMap e a -> [(e, a)]
      postSetWithLabel v g =
        let es = toList $ AM.postSet v g
         in es <&> \v1 ->
              (,v1) $ AM.edgeLabel v v1 g

-- | Create a patch graph that that will "copy" the given graph when applied as
-- a patch to an empty graph.
asPatchGraph :: AM.AdjacencyMap e v -> PatchGraph e v
asPatchGraph am =
  let addVertices =
        AM.vertexList am <&> flip ModifyGraph_ReplaceVertexWithSuccessors mempty
      addEdges =
        AM.edgeList am <&> (\(e, v1, v2) -> ModifyGraph_AddEdge e v1 v2)
   in PatchGraph $ addVertices <> addEdges

-- | Return the vertices in the graph with the given pruning function.
--
-- Use this function to accomodate for PatchGraph's idiosyncratic behaviour of
-- leaving orphans behind.
--
-- Prunes only orphan vertices. i.e., the pruning function cannot prune vertices
-- with non-zero edges.
patchedGraphVertexSet :: forall e v. (Ord v) => (v -> Bool) -> AM.AdjacencyMap e v -> Set v
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
    connectedVertices :: AM.AdjacencyMap e v -> Set v
    connectedVertices =
      Set.fromList . concatMap (\(_e, v1, v2) -> [v1, v2]) . AM.edgeList
