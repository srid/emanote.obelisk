module Emanote.Graph where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Emanote.Markdown.WikiLink (WikiLinkContext, WikiLinkID, WikiLinkLabel)

type V = WikiLinkID

type E = [(WikiLinkLabel, WikiLinkContext)]

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
