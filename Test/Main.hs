{-|
Entry point for the test suite, aggregates all unit tests and runs
them.
-}
module Main (main) where

import Test.Framework
import Test.StronglyConnectedComponents
import Test.Condensation
import Test.TopologicalSort
import Test.GraphUtils
import Test.LongestPath

-- | Test suite entry poing, aggregates tests from all modules.
main :: IO ()
main = defaultMain 
       [testGraphUtils
       ,testSCC
       ,testCondensation
       ,testTopologicalSort
       ,testLongestPath]
