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

pIsTopological :: Graph -> Bool
pIsTopological graph =
  let
    condensationGraph = condensation graph    
    sorting = topologicalSort condensationGraph in
  isTopological condensationGraph sorting

isTopological :: Graph -> [Vertex] -> Bool
isTopological graph ordering =
  all (\(u,v) -> u `comesBefore` v) $ edges graph
  where u `comesBefore` v = ordering !! u <= ordering !! v
