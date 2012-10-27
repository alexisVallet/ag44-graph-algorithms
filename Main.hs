module Main (main) where

import GraphIO
import StronglyConnectedComponents as SCC
import Condensation as Condensation
import LongestPath as LongestPath

import System.Console.CmdArgs

data GraphAlgorithmsModes = SCCs { file :: FilePath }
                          | Condensation { file :: FilePath }
                          | LongestPath { file :: FilePath }
                            deriving (Show, Data, Typeable)

sccsMode = SCCs {file = def}
condensationMode = Condensation {file = def}
longestPathMode = LongestPath {file = def}

main :: IO ()
main = do
  args <- cmdArgs (modes [sccsMode, condensationMode, longestPathMode])
  graph <- fromFile $ file args
  case args of
    SCCs _ -> showGraphAndSCC graph (tarjan graph)
    _ -> return ()