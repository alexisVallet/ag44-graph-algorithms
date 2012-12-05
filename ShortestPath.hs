{-|
This module provides shortest path computation using
Dijkstra's shortest path algorithm. Dijkstra's algorithm
only works on graphs with no negative weights
-}
module ShortestPath (dijkstra) where

import GraphUtils
import WeightedGraph
import VectorUtils

import Prelude hiding (read, replicate)
import Control.Monad.State hiding (modify)
import Control.Monad.Cont
import Data.PSQueue
import Control.Monad.ST
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Data.Vector.Mutable
import Data.Maybe

data Distance = Infinite | Finite Float
              deriving (Eq)

instance Ord Distance where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite f1) (Finite f2) = compare f1 f2

data DijkstraVertexData = DVertexData {
  _distance :: Distance,
  _predecessor :: Maybe Vertex
  }
makeLenses ''DijkstraVertexData

data DijkstraState s = DState {
  _verticesData :: STVector s DijkstraVertexData,
  _verticesPQueue :: PSQ Vertex Distance
  }
makeLenses ''DijkstraState

type DijkstraMonad s = StateT (DijkstraState s) (ST s)

runDijkstraMonad :: 
  WeightedGraph -> (forall s . DijkstraMonad s a) -> a
runDijkstraMonad wgraph action = runST $ do
  let size = order $ graph wgraph
  initialVerticesData <- 
    replicate size (DVertexData Infinite Nothing)
  evalStateT action 
    $ DState initialVerticesData 
    $ fromAscList 
    $ zipWith (:->) [0..size-1] (repeat Infinite)

-- | Finds the shortest path in a weighted graph between
-- two vertices.
dijkstra :: WeightedGraph -> Vertex -> Vertex -> [Vertex]
dijkstra wgraph source destination = 
  runDijkstraMonad wgraph $ dijkstra' wgraph source destination

dijkstra' :: WeightedGraph -> Vertex -> Vertex -> DijkstraMonad s [Vertex]
dijkstra' wgraph source destination = do
  lift $ modifyL verticesData source $ distance .~ Finite 0
  verticesPQueue %= adjust (const 0) source
  flip runContT (const $ return ()) $ callCC $ \exit -> forever $ do
    verticesPQueue' <- use verticesPQueue
    case findMin verticesPQueue' of
      Nothing -> exit () -- The priority queue is empty
      Just (_ :-> Infinite) -> exit () -- The destination is unreachable
      Just (vertex :-> Finite vertexDistance) -> do
        verticesPQueue %= deleteMin
        when (vertex == destination) $ exit () -- The destination vertex has been found
        forM_ (successors (graph wgraph) vertex) $ \successor -> do
          let arcDistance = 
                fromJust $ weight wgraph (vertex, successor)
          successorData <- lift $ readL verticesData successor
          let newDistance = arcDistance + vertexDistance
          when (Finite newDistance < successorData ^. distance) $ do
            lift $ writeL verticesData successor 
              $ DVertexData (Finite newDistance) (Just vertex)
            verticesPQueue %= adjust (const $ Finite newDistance) vertex
  fmap reverse $ reconstructPathFrom destination

reconstructPathFrom :: Vertex -> DijkstraMonad s [Vertex]
reconstructPathFrom vertex = do
  vertexData <- lift $ readL verticesData vertex
  case vertexData ^. predecessor of
    Nothing -> return [vertex]
    Just predecessor -> do
      rest <- reconstructPathFrom predecessor
      return $ vertex:rest
