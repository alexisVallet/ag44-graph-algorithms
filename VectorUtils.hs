{-|
This module provide some utility functions for manipulating mutable
vectors.
-}
module VectorUtils 
       (
         modifyL,
         readL,
         writeL
       ) where

import Prelude hiding (read)
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.State.Class hiding (modify)
import Control.Monad.Primitive
import Control.Lens
import Data.Vector.Generic.Mutable

modifyL :: (MonadTrans t, PrimMonad m, MonadState st (t m), MVector v a)
           => Getting (v (PrimState m) a) st (v (PrimState m) a)
           -> Int -> (a -> a) -> t m ()
modifyL vectorLens index modification = do
  vector <- use vectorLens
  element <- lift $ read vector index
  lift $ write vector index (modification element)

readL :: (MonadTrans t, PrimMonad m, MonadState st (t m), MVector v a)
         => Getting (v (PrimState m) a) st (v (PrimState m) a)
         -> Int -> t m a
readL vectorLens index = do
  vector <- use vectorLens
  lift $ read vector index

writeL :: (MonadTrans t, PrimMonad m, MonadState st (t m), MVector v a)
          => Getting (v (PrimState m) a) st (v (PrimState m) a)
          -> Int -> a -> t m ()
writeL vectorLens index newElement = do
  vector <- use vectorLens
  lift $ write vector index newElement
