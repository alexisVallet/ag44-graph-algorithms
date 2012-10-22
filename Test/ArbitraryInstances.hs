module Test.ArbitraryInstances where

import Test.QuickCheck
import Data.List
import Data.Graph
import Data.Array

maxNumberOfVertices :: Int
maxNumberOfVertices = 100

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
