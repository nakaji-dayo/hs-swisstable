module Test.Basic where

import           Control.Monad
import           Lib
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Primitive.Array     as A
import           Data.Primitive.PrimArray



unit_insertAndLookup :: IO ()
unit_insertAndLookup = do
  let ks = ["A","Z", "C", "Y", "E", "X", "G", "W"]
  t <- new
  mapM_ (`insert` t) ks
  ia <- freezeArray  (elems t) 0 (sizeofMutableArray $ elems t)
  print ia
  ictrl <- freezePrimArray (ctrl t) 0 initialCap
  print ictrl
  print  "-----------"
  forM_ ks $ \k -> do
    h <- lookup'' k t
    print (k, h)
    (Just k) @=? h

unit_grow_rehash :: IO ()
unit_grow_rehash = do
  let ks = ["A","Z", "C", "Y", "E", "X", "G", "W", "ab", "cd", "ef", "gh", "xx"]
  t <- new
  mapM_ (`insert` t) ks
  ia <- freezeArray  (elems t) 0 (sizeofMutableArray $ elems t)
  print ia
  ictrl <- freezePrimArray (ctrl t) 0 initialCap
  print ictrl
  print  "-----------"
  forM_ ks $ \k -> do
    h <- lookup'' k t
    print (k, h)
    (Just k) @=? h
