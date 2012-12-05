{-|
This module provides a data structure for directed graphs
with weighted edges.
-}
module WeightedGraph 
       (
         WeightedGraph(..), 
         LabelledGraph(..), 
         weight
       ) where

import Prelude hiding (foldr, lookup)
import Data.Map hiding (foldr)
import Data.Foldable

import GraphUtils

-- | Graph with labels associated to edges.
data LabelledGraph a = LGraph {
  graph :: Graph,
  labels :: Map (Vertex, Vertex) a
  }

type WeightedGraph = LabelledGraph Float

instance Foldable LabelledGraph where
  foldr f initial (LGraph _ labels) =
    foldr f initial labels

instance Functor LabelledGraph where
  fmap f (LGraph graph labels) =
    LGraph graph $ fmap f labels

weight :: WeightedGraph -> (Vertex, Vertex) -> Maybe Float
weight (LGraph _ labels) (s,d) =
  lookup (s,d) labels
