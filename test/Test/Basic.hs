module Test.Basic where

import           Prelude                  hiding (lookup)

import           Control.Monad
import           Lib
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Primitive.Array     as A
import           Data.Primitive.PrimArray


unit_insertAndLookup :: IO ()
unit_insertAndLookup = do
  let ks = ["A", "Z", "C", "Y", "E", "X", "G", "W"]
  t <- new
  mapM_ (\k -> insert k k t) ks
  -- ia <- freezeArray  (elems t) 0 (sizeofMutableArray $ elems t)
  -- print ("test1", ia)
  -- ictrl <- freezePrimArray (ctrl t) 0 initialCap
  -- print ictrl
  -- print  "-----------"
  forM_ ks $ \k -> do
    h <- lookup k t
    Just k @=? h

unit_insert_conflict :: IO ()
unit_insert_conflict = do
  let ks = ["head", "Z", "C", "last"]
  t <- newSized 8
  mapM_ (\x -> insert' h x x t) ks
  forM_ ks $ \k -> do
    h <- lookup' h k t
    Just k @=? h
 where
   h = const 0

unit_insert_right_overflow :: IO ()
unit_insert_right_overflow = do
  let ks = ["head", "Z", "C", "last"]
  t <- newSized 8
  mapM_ (\x -> insert' h x x t) ks
  forM_ ks $ \k -> do
    h <- lookup' h k t
    Just k @=? h
 where
   h = const 7

unit_lookup_nothing_conflict :: IO ()
unit_lookup_nothing_conflict = do
  let ks = ["A", "B", "C", "D"]
  t <- new
  mapM_ (\x -> insert' h x x t) ks
  h <- lookup' h "X" t
  h @=? Nothing
 where
   h = const 7

unit_lookup_nothing :: IO ()
unit_lookup_nothing = do
  let ks = ["A", "B", "C", "D"]
  t <- new
  mapM_ (\x -> insert x x t) ks
  h <- lookup "X" t
  h @=? Nothing

unit_grow_rehash :: IO ()
unit_grow_rehash = do
  let ks = ["A","Z", "C", "Y", "E", "X", "G", "W", "ab", "cd", "ef", "gh", "xx"]
  t <- newSized 8
  mapM_ (\k -> insert k k t) ks
  forM_ ks $ \k -> do
    h <- lookup k t
    Just k @=? h

unit_grow_rehash2 :: IO ()
unit_grow_rehash2 = do
  let ks = [0..99::Int]
  t <- newSized 8
  mapM_ (\k -> insert k k t) ks
  forM_ ks $ \k -> do
    h <- lookup k t
    Just k @=? h

unit_delete :: IO ()
unit_delete = do
  let ks = ["A","B", "C"]
  t <- new
  mapM_ (\k -> insert k k t) ks
  delete "B" t
  h <- lookup "B" t
  h @=? Nothing
  h <- lookup "C" t
  h @=? Just "C"
