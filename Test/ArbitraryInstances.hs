module Test.ArbitraryInstances where

import Test.QuickCheck
import Data.List
import Data.Graph
import Data.Array

maxNumberOfVertices :: Int
maxNumberOfVertices = 10

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

-- | Newtype wrapper for directed acyclic graphs, for a different
-- arbitrary instance.
newtype DAG = DAG Graph

-- | Adjacency matrix arbitrary instance
instance Arbitrary (Array (Vertex,Vertex) Bool) where
  arbitrary = do
    size <- choose (1, maxNumberOfVertices)
    let bounds' = ((0,0),(size-1,size-1))
    elements <- vectorOf (size*size) arbitrary :: Gen [Bool]
    return $ listArray bounds' elements