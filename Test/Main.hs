module Main (main) where

import Test.Framework
import Test.StronglyConnectedComponents
import Test.TopologicalSort

main :: IO ()
main = defaultMain 
       [testSCC
       ,testTopologicalSort]