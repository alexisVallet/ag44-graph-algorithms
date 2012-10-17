module StronglyConnectedComponents
       (
         fromFile,
         tarjan
       ) where

import Data.Graph (
  Graph(..),
  vertices,
  edges,
  Vertex(..), 
  Edge(..), 
  Table(..), 
  Bounds(..))
import Prelude as Prelude
import System.IO as IO
import Data.ByteString as ByteString
import Data.Array as Array hiding (index)
import Data.Char (ord)
import Data.Word
import Control.Monad.ST
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Class
import Data.Vector.Mutable as Vector
import Data.Lenses.Template as Lenses
import Control.Monad.Primitive
import Control.Monad
import Data.Maybe

fromFile :: FilePath -> IO Graph
fromFile filePath = do
  rawData <- ByteString.readFile filePath
  return $ rawDataToGraph rawData

toWord :: Char -> Word8
toWord = fromIntegral . ord

rawDataToGraph :: ByteString -> Graph
rawDataToGraph rawData =
  let lines = ByteString.split (toWord '\n') rawData
      adjacencyLists = Prelude.map lineToAdjList lines
      numberOfVertices = Prelude.length lines in
  listArray
  (0,numberOfVertices-1) 
  adjacencyLists

lineToAdjList :: ByteString -> [Vertex]
lineToAdjList line =
  snd 
  $ ByteString.foldr
  (\word (index,vertices) -> 
    let isZero = word == toWord '0'
        isOne = word == toWord '1'
        newIndex = if isZero || isOne then index + 1 else index
        newVertices = if isOne then index:vertices else vertices in
    (newIndex, newVertices))
  (0,[])
  line

data SCCVertexData = SCCVertexData {
  vertIndex_ :: Int,
  vertLowLink_ :: Int
  }
$(deriveLenses ''SCCVertexData)

data SCCState s = SCCState {
  globalIndex_ :: Int,
  verticesData_ :: MVector (PrimState (ST s)) (Maybe SCCVertexData),
  visitedStack_ :: [Vertex]
  }
$(deriveLenses ''SCCState)

type SCC s = RWST Graph [[Vertex]] (SCCState s) (ST s)

runSCC :: Graph -> (forall s. SCC s a) -> [[Vertex]]
runSCC graph sccAction = runST $ do
  let (min,max) = bounds graph
      numberOfVertices = max - min + 1
  initialVertexData <- Vector.replicate numberOfVertices Nothing
  let initialState = SCCState 0 initialVertexData []
  fmap snd $ evalRWST sccAction graph initialState

getVertexData :: Vertex -> SCC s (Maybe SCCVertexData)
getVertexData vertex = do
  verticesData' <- gets verticesData
  lift $ Vector.read verticesData' vertex

hasNotBeenVisited :: Vertex -> SCC s Bool
hasNotBeenVisited vertex = do
  vertexData <- getVertexData vertex
  return $ isNothing vertexData

tarjan :: Graph -> [[Vertex]]
tarjan graph = runSCC graph tarjanSCC

tarjanSCC :: SCC s ()
tarjanSCC = do
  graph <- ask
  forM_ (vertices graph) $ \vertex -> do
    notVisited <- hasNotBeenVisited vertex
    when notVisited $ do
      strongConnect vertex

strongConnect :: Vertex -> SCC s ()
strongConnect vertex = return ()
