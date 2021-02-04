module Emanote.Graph where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Emanote.Markdown.WikiLink (Directed (..), WikiLinkContext, WikiLinkID, WikiLinkLabel)

type V = WikiLinkID

type E' = (WikiLinkLabel, WikiLinkContext)

type E = [E']

newtype Graph = Graph {unGraph :: AM.AdjacencyMap E V}
  deriving (Eq, Show)

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

connectionsOf :: (Directed WikiLinkLabel -> Bool) -> V -> Graph -> [(E', V)]
connectionsOf f v graph =
  go UserDefinedDirection postSetWithLabel
    <> go ReverseDirection preSetWithLabel
  where
    go dir pSet =
      concat $
        flip fmap (pSet v (unGraph graph)) $ \(es, t) ->
          flip mapMaybe es $ \(lbl, ctx) -> do
            guard $ f (dir lbl)
            pure ((lbl, ctx), t)

-- | Filter the graph to contain only edges satisfying the predicate
filterBy :: (Directed WikiLinkLabel -> Bool) -> Graph -> Graph
filterBy f graph =
  Graph $ AM.edges $ mapMaybe g $ AM.edgeList $ unGraph graph
  where
    g (lbls, v1, v2) = do
      let lbls' = filter (f . UserDefinedDirection . fst) lbls
      guard $ not $ null lbls'
      pure (lbls', v1, v2)
