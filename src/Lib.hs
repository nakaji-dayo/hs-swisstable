{-# LANGUAGE LambdaCase #-}
module Lib where

import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr

import           Data.Char             (intToDigit)
import           Numeric               (showHex, showIntAtBase)

import           Control.Monad
import           Data.Bits
import           Data.Hashable

-- import qualified Data.Vector           as V
-- import           Data.Vector.Mutable   (MVector)
-- import qualified Data.Vector.Mutable   as V

import           Data.Primitive.Array  as A

foreign import ccall "_elm_cmp_vec" cElmCmpVec :: Word8 -> Ptr Word8 -> Word


f = do
  src <- mallocArray 32
  pokeArray src [0..31]
  let res = cElmCmpVec 5 src
  putStrLn $ showIntAtBase 2 intToDigit res ""

initialCap = 8

data Table s k = Table
 { ctrl :: MutableArray s k
 , size :: Int
 }

testks = ["A","Z", "C", "Y", "E", "X", "G", "W"]

test :: IO ()
test = do
  c <- A.newArray initialCap (Nothing :: Maybe String)
  let t = Table c (fromIntegral initialCap)
  mapM_ (`insert` t) testks
  ia <- freezeArray  (ctrl t) 0 (sizeofMutableArray $ ctrl t)
  print ia
  print  "-----------"
  forM_ testks $ \k -> do
    h <- lookup' k t
    print (k, h)

insert k m = do
  let h = hash k
  let idx = (size m - 1) .&. h
  print k
  f idx
  where
    f idx =
      readArray (ctrl m) idx >>= \case
        Just x -> print "shift" >>f (idx + 1)
        Nothing ->
          writeArray (ctrl m) idx (Just k)

lookup' k m = do
  let h = hash k
  let idx = (size m - 1) .&. h
  print k
  f idx
  where
    f idx =
      readArray (ctrl m) idx >>= \case
        Just x
          | x == k -> pure (Just k)
          | otherwise -> f (idx + 1)
        Nothing -> pure Nothing
