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

import           Data.Primitive
import           Data.Primitive.Array     as A
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr       as PP
import           Debug.Trace



foreign import ccall "_elm_cmp_vec" cElmCmpVec :: Word8 -> Ptr Word8 -> Word32
foreign import ccall "ffs" cFfs :: Word32 -> Int


showB x = showIntAtBase 2 intToDigit x ""

f = do
  src <- mallocArray 32
  pokeArray src [0..31]
  let res = cElmCmpVec 5 src
  putStrLn $ showB res

initialCap = 8

data Table s k = Table
 { elems :: MutableArray s (Maybe k)
 , ctrl  :: MutablePrimArray s Word8
 , size  :: Int
 }

new = do
  es <- A.newArray initialCap Nothing
  c <- newPinnedPrimArray (initialCap + 32)
  setPrimArray c 0 initialCap 128
  print 8
  pure $ Table es c (fromIntegral initialCap)

insert k m = do
  let h = hash k
  let idx = (size m - 1) .&. h
  print k
  f idx
  where
    f idx =
      readArray (elems m) idx >>= \case
        Just x -> print "shift" >>f (idx + 1)
        Nothing -> do
          writeArray (elems m) idx (Just k)
          writePrimArray (ctrl m) idx (h2 k)

lookup' k m = do
  let h = hash k
  let idx = (size m - 1) .&. h
  print k
  f idx
  where
    f idx =
      readArray (elems m) idx >>= \case
        Just x
          | x == k -> pure (Just k)
          | otherwise -> f (idx + 1)
        Nothing -> pure Nothing

lookup'' k m = do
  let h1' = h1 k
      h2' = h2 k
  let idx = (size m - 1) .&. h1'
  let pc = PP.advancePtr (mutablePrimArrayContents (ctrl m)) idx
  let mask = cElmCmpVec h2' pc
  let idx' = idx + cFfs mask - 1
  readArray (elems m) idx'

h1 :: String -> Int
h1 = hash
h2 :: Hashable a => a -> Word8
h2 x = fromIntegral $ hash x .&. 127
