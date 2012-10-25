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

pIsTopological :: DAG -> Property
pIsTopological (DAG graph) =
  let
    sorting = topologicalSort graph
    showResults = do
      putStrLn $ "Sorting: " ++ show sorting
      in
  whenFail showResults
  $ isTopological graph sorting

isTopological :: Graph -> [Vertex] -> Bool
isTopological graph ordering =
  all (\(u,v) -> u `comesBefore` v) $ edges graph
  where u `comesBefore` v =
          case (findIndex (==u) ordering,findIndex (==v) ordering) of
            (Just i,Just j) -> i < j
            _ -> error "The ordering is not a permutation!"
