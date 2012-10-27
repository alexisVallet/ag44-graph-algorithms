{-|
Provides artbitrary instances for adjacency lists, adjacency matrices
and directed acyclic graphs as adjacency lists.
-}
module Test.ArbitraryInstances where

import Test.QuickCheck
import Data.List
import Data.Graph
import Data.Array
import Control.Monad.List

import GraphUtils

-- | Maximum number of vertices for the graphs generated.
maxNumberOfVertices :: Int
maxNumberOfVertices = 100

-- | Adjacency list arbitrary instance.
instance Arbitrary (Array Int [Int]) where
  arbitrary = do
    size <- choose (0, maxNumberOfVertices) :: Gen Int
    adjLists <- 
      vectorOf size 
      $ fmap nub 
      $ resize maxNumberOfVertices 
      $ listOf 
      $ choose (0, size-1)
    return $ listArray (0,size-1) adjLists

-- | Adjacency matrix arbitrary instance
instance Arbitrary (Array (Vertex,Vertex) Bool) where
  arbitrary = do
    size <- choose (1, maxNumberOfVertices)
    let bounds' = ((0,0),(size-1,size-1))
    elements <- vectorOf (size*size) arbitrary :: Gen [Bool]
    return $ listArray bounds' elements

-- | Newtype wrapper for directed acyclic graphs, for a different
-- arbitrary instance.
newtype DAG = DAG Graph deriving (Show)

-- | Generates random directed acyclic graphs by generating
-- lower triangular random adjacency matrices. Inspired
-- by http://mathematica.stackexchange.com/questions/608/how-to-generate-random-directed-acyclic-graphs
instance Arbitrary DAG where
  arbitrary = do
    size <- choose (1, maxNumberOfVertices)
    let indices = [0..size-1]
    assocList <- do
      forM [(i,j) | i <- indices, j <- indices] $ \(i,j) -> do
        if j >= i
          then return ((i,j),False)
          else do
            randomEdge <- arbitrary :: Gen Bool
            return ((i,j),randomEdge)
    return 
      $ DAG 
      $ adjacencyList 
      $ array ((0,0),(size-1,size-1)) assocList
