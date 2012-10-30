module Main (main) where

import Criterion.Main

import Bench.StronglyConnectedComponents

main :: IO ()
main = defaultMain
       [benchSCCs]