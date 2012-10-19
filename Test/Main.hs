module Main (main) where

import Test.Framework
import Test.StronglyConnectedComponents

main :: IO ()
main = defaultMain [testSCC]