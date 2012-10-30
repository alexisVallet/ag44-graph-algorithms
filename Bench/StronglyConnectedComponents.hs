module Bench.StronglyConnectedComponents (benchSCCs) where

import Criterion.Main

import Bench.Graphs
import Control.Monad.Random
import System.Random
import StronglyConnectedComponents
import qualified Data.Graph as Graph

benchSCCs :: Benchmark
benchSCCs =
  let
    (smallGraph, smallishGraph, mediumGraph, largeishGraph, largeGraph) = 
      flip evalRand (mkStdGen 0) $ do
        small <- randomGraphOfSize 100 1000
        smallish <- randomGraphOfSize 300 3000
        medium <- randomGraphOfSize 500 5000
        largeish <- randomGraphOfSize 700 7000
        large <- randomGraphOfSize 900 9000
        return (small, smallish, medium, largeish, large) in
  bcompare
  [bgroup "Data.Graph.scc"
   [bench "100 vertices" $ nf Graph.scc smallGraph,
    bench "300 vertices" $ nf Graph.scc smallishGraph,
    bench "500 vertices" $ nf Graph.scc mediumGraph,
    bench "700 vertices" $ nf Graph.scc largeishGraph,
    bench "900 vertices" $ nf Graph.scc largeGraph],
   bgroup "tarjan"
   [bench "100 vertices" $ nf tarjan smallGraph,
    bench "300 vertices" $ nf tarjan smallishGraph,
    bench "500 vertices" $ nf tarjan mediumGraph,
    bench "700 vertices" $ nf tarjan largeishGraph,
    bench "900 vertices" $ nf tarjan largeGraph]]