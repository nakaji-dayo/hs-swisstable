{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Lib where

import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr

import           Data.Char                (intToDigit)
import           Numeric                  (showHex, showIntAtBase)

import           Control.Monad
import           Data.Bits
import           Data.Hashable

-- import qualified Data.Vector           as V
-- import           Data.Vector.Mutable   (MVector)
-- import qualified Data.Vector.Mutable   as V

-- import qualified Data.Vector           as V
-- import           Data.Vector.Mutable   (MVector)
-- import qualified Data.Vector.Mutable   as V
import           Control.Monad.Primitive  (PrimMonad (PrimState), RealWorld)
import           Data.Primitive
import           Data.Primitive.Array     as A
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr       as PP
import           Debug.Trace



foreign import ccall "_elm_cmp_vec" cElmCmpVec :: Word8 -> Ptr Word8 -> Word32
foreign import ccall "_load_movemask" cLoadMovemask :: Ptr Word8 -> Word32
foreign import ccall "ffs" cFfs :: Word32 -> Int


showB x = showIntAtBase 2 intToDigit x ""

f = do
  src <- mallocArray 32
  pokeArray src [0..31]
  let res = cElmCmpVec 5 src
  putStrLn $ showB res

data Table s k = Table
 { elems :: MutableArray s (Maybe k)
 , ctrl  :: MutablePrimArray s Word8
 , size  :: Int
 }

new :: PrimMonad m => m (Table (PrimState m) k)
new = newSized 8

newSized :: PrimMonad m => Int -> m (Table (PrimState m) k)
newSized n = do
  es <- A.newArray n Nothing
  c <- newPinnedPrimArray (n + 32)
  setPrimArray c 0 n 128
  pure $ Table es c (fromIntegral n)

insert' :: (PrimMonad m, Hashable k) => (k -> Int) -> k -> Table (PrimState m) k -> m ()
insert' hash' k m = do
  let h1' = h1 hash' k
  let idx = (size m - 1) .&. h1'
  let pc = PP.advancePtr (mutablePrimArrayContents (ctrl m)) idx
  let mask = cLoadMovemask pc
  let idx' = idx + cFfs mask - 1
  writeArray (elems m) idx' (Just k)
  writePrimArray (ctrl m) idx' (h2 hash' k)

insert :: (PrimMonad m, Hashable k) => k -> Table (PrimState m) k -> m ()
insert = insert' hash

lookup :: (PrimMonad m, Hashable k)
  => k -> Table (PrimState m) k -> m (Maybe k)
lookup = lookup' hash

lookup' :: (PrimMonad m, Hashable k)
  => (k -> Int) -> k -> Table (PrimState m) k -> m (Maybe k)
lookup' hash' k m = do
  let h1' = h1 hash' k
      h2' = h2 hash' k
  let idx = (size m - 1) .&. h1'
  let pc = PP.advancePtr (mutablePrimArrayContents (ctrl m)) idx
  let mask = cElmCmpVec h2' pc
  let idx' = idx + cFfs mask - 1
  readArray (elems m) idx'

h1 :: Hashable k => (k -> Int) -> k -> Int
h1 = ($)

h2 :: Hashable a => (a -> Int) -> a -> Word8
h2 h x = fromIntegral $ h x .&. 127

delete = undefined
