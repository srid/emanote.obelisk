module Emanote.Graph where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Set as Set
import Emanote.Markdown.WikiLink (Directed (..), WikiLink, WikiLinkID, WikiLinkLabel)
import qualified Emanote.Markdown.WikiLink as W
import Relude

type V = WikiLinkID

type E' = WikiLink

-- TODO: Document why Set?
type E = Set E'

newtype Graph = Graph {unGraph :: AM.AdjacencyMap E V}
  deriving (Eq, Show)

empty :: Graph
empty = Graph AM.empty

-- TODO: Rename to `neighbours` and make generic (on AdjacencyMap)
connectionsOf :: (Directed WikiLinkLabel -> Bool) -> V -> Graph -> [(E', V)]
connectionsOf f x graph =
  go UserDefinedDirection postSetWithLabel
    <> go ReverseDirection preSetWithLabel
  where
    go dir pSet =
      mconcat $
        flip fmap (pSet x (unGraph graph)) $ \(es, t) -> do
          flip mapMaybe (Set.toList es) $ \wl -> do
            guard $ f (dir $ W._wikilink_label wl)
            pure (wl, t)
    postSetWithLabel :: (Ord a, Monoid e) => a -> AM.AdjacencyMap e a -> [(e, a)]
    postSetWithLabel v g =
      let vs = toList $ AM.postSet v g
       in vs <&> \v2 ->
            (,v2) $ AM.edgeLabel v v2 g

    preSetWithLabel :: (Ord a, Monoid e) => a -> AM.AdjacencyMap e a -> [(e, a)]
    preSetWithLabel v g =
      let vs = toList $ AM.preSet v g
       in vs <&> \v0 ->
            (,v0) $ AM.edgeLabel v0 v g

-- | Filter the graph to contain only edges satisfying the predicate
filterBy :: (Directed WikiLinkLabel -> Bool) -> Graph -> Graph
filterBy f graph =
  Graph $ AM.edges $ mapMaybe g $ AM.edgeList $ unGraph graph
  where
    g (wls :: Set WikiLink, v1, v2) = do
      let wls' = Set.filter (f . UserDefinedDirection . W._wikilink_label) wls
      guard $ not $ Set.null wls'
      pure (wls', v1, v2)
