module Main (main) where

import Test.Framework
import Test.StronglyConnectedComponents
import Test.Condensation
import Test.TopologicalSort
import Test.GraphUtils
import Test.LongestPath

main :: IO ()
main = defaultMain 
       [testGraphUtils
       ,testSCC
       ,testCondensation
       ,testTopologicalSort
       ,testLongestPath]
