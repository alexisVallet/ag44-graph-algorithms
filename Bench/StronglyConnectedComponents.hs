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
    (smallGraph, mediumGraph, largeGraph) = 
      flip evalRand (mkStdGen 0) $ do
        small <- randomGraphOfSize 50 500
        medium <- randomGraphOfSize 500 5000
        large <- randomGraphOfSize 1000 10000
        return (small, medium, large) in
  bgroup "Comparison between tarjan and scc"
  [bcompare
   [bench "Data.Graph.scc: small graph" $ nf Graph.scc smallGraph,
    bench "tarjan: small graph" $ nf tarjan smallGraph],
   bcompare
   [bench "Data.Graph.scc: medium graph" $ nf Graph.scc mediumGraph,
    bench "tarjan: medium graph" $ nf tarjan mediumGraph],
   bcompare
   [bench "Data.Graph.scc: large graph" $ nf Graph.scc largeGraph,
    bench "tarjan: large graph" $ nf tarjan largeGraph]]
