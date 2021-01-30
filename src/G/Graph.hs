{-# LANGUAGE TypeFamilies #-}

module G.Graph where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import G.Markdown.WikiLink (WikiLinkID)
import Reflex.Patch.Class (Patch (..))

data Label
  = Unlabelled
  | Labelled [Text]
  deriving (Eq, Ord, Show)

instance Semigroup Label where
  Unlabelled <> Unlabelled = Unlabelled
  Labelled s <> Unlabelled = Labelled s
  Unlabelled <> Labelled s = Labelled s
  Labelled s1 <> Labelled s2 = Labelled (s1 <> s2)

type V = WikiLinkID

newtype Graph = Graph {unGraph :: AM.AdjacencyMap (Maybe Label) V}
  deriving (Eq, Show)

newtype PatchGraph = PatchGraph {unPatchGraph :: Map V (Maybe [V])}

instance Patch PatchGraph where
  type PatchTarget PatchGraph = Graph
  apply (PatchGraph p) (Graph graph) =
    Graph <$> patch (fmap (fmap (fmap (Just Unlabelled,))) p) graph
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
              then pure False
              else do
                -- Remove all edges, then add new ones back in.
                forM_ esOld $ \(_, v2) ->
                  modify $ AM.removeEdge v v2
                modify $
                  AM.overlay $
                    AM.edges $
                      (\(e, v1) -> (e, v, v1)) <$> es
                pure True

empty :: Graph
empty = Graph AM.empty

postSetWithLabel :: (Ord a, Monoid e) => a -> AM.AdjacencyMap e a -> [(e, a)]
postSetWithLabel v g =
  let es = toList $ AM.postSet v g
   in es <&> \v1 ->
        (,v1) $ AM.edgeLabel v v1 g

preSetWithLabel :: (Ord a, Monoid e) => a -> AM.AdjacencyMap e a -> [(e, a)]
preSetWithLabel v g =
  let es = toList $ AM.preSet v g
   in es <&> \v0 ->
        (,v0) $ AM.edgeLabel v0 v g
