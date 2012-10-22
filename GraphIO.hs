module GraphIO 
       (
         showGraphAndSCC
       ) where

import Data.Graph
import Data.GraphViz
import Data.List
import Data.Maybe

graphAndSCCToDot :: Graph -> [[Vertex]] -> DotGraph Vertex
graphAndSCCToDot graph sccs =
  let graphVizParams = Params {
        isDirected = True,
        globalAttributes = [],
        clusterBy = \(node,scc) -> C scc $ N (node,node),
        clusterID = id,
        fmtCluster = const [],
        fmtNode = const [],
        fmtEdge = const []
        }
      sccContaining node = 
        case findIndex (elem node) sccs of
          Just i -> i
          Nothing -> error $ "Node " ++ show node ++ " does not belong to any SCC!"
      nodeList =
         map (\node -> (node, Int $ sccContaining node)) 
         $ vertices graph
      edgeList = map (\(i,j) -> (i,j,())) $ edges graph in
   graphElemsToDot graphVizParams nodeList edgeList

showGraphAndSCC :: Graph -> [[Vertex]] -> IO ()
showGraphAndSCC graph sccs = do
  graphvizInstalled <- isGraphvizInstalled
  if graphvizInstalled 
    then do
      let dotGraph = graphAndSCCToDot graph sccs
      putStrLn $ "The dot graph is: " ++ show dotGraph
      runGraphvizCanvas' dotGraph Xlib
    else do
      putStrLn "Cannot show the graph, please install graphviz."