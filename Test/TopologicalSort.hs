{-|
Unit test module for the topological sorting algorithm. Verifies
that the algorithm returns a valid topological sorting against
randomly generated DAGs.
-}
module Test.TopologicalSort (testTopologicalSort) where

import GraphUtils
import TopologicalSort
import Condensation
import Test.ArbitraryInstances

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.List

testTopologicalSort :: Test
testTopologicalSort =
  testProperty "Returns a valid topological sorting of the graph condensation" pIsTopological

-- | Property holding true iff the topological sorting algorithm
-- returns a valid topological sorting. 
pIsTopological :: DAG -> Property
pIsTopological (DAG graph) =
  let
    sorting = topologicalSort graph
    showResults = do
      putStrLn $ "Sorting: " ++ show sorting
      in
  whenFail showResults
  $ isTopological graph sorting

-- | Returns true iff the ordering is topological in the graph,
-- by checking every edge in the graph. Also fails when the ordering
-- is not a permutation of the graph vertices.
isTopological :: Graph -> [Vertex] -> Bool
isTopological graph ordering =
  all (\(u,v) -> u `comesBefore` v) $ edges graph
  where u `comesBefore` v =
          case (findIndex (==u) ordering,findIndex (==v) ordering) of
            (Just i,Just j) -> i < j
            _ -> error "The ordering is not a permutation!"
