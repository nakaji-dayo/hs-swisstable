module Test.Basic where

import           Prelude                  hiding (lookup)

import           Control.Monad
import           IO
import qualified Lib                      as H
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad.ST         (stToIO)
import           Data.Primitive.Array     as A
import           Data.Primitive.PrimArray
import           Test.QuickCheck          (Gen, generate, vector)


unit_insertAndLookup :: IO ()
unit_insertAndLookup = do
  let ks = ["A", "Z", "C", "Y", "E", "X", "G", "W"]
  ref <- new
  mapM_ (\k -> insert ref k k) ks
  -- t <- stToIO $ H.readRef ref
  -- ia <- freezeArray  (H.elems t) 0 (sizeofMutableArray $ H.elems t)
  -- print ("test1", ia)
  -- ictrl <- freezePrimArray (H.ctrl t) 0 (H.size t)
  -- print ictrl
  -- print  "-----------"
  forM_ ks $ \k -> do
    h <- lookup ref k
    Just k @=? h

unit_insertAndLookup_rand :: IO ()
unit_insertAndLookup_rand = do
  ks <- generate (vector 100 :: Gen [Int])
  ref <- new
  mapM_ (\k -> insert ref k k) ks
  forM_ ks $ \k -> do
    h <- lookup ref k
    Just k @=? h

-- unit_large_space :: IO ()
-- unit_large_space =
--   void $ newSized (2^32)

unit_insert_conflict :: IO ()
unit_insert_conflict = do
  let ks = ["head", "Z", "C", "last"]
  t <- newSized 8
  mapM_ (\x -> insert' h t x x) ks
  forM_ ks $ \k -> do
    h <- lookup' h t k
    Just k @=? h
 where
   h = const 0

unit_insert_right_overflow :: IO ()
unit_insert_right_overflow = do
  let ks = ["head", "Z", "C", "last"]
  t <- newSized 8
  mapM_ (\x -> insert' h t x x) ks
  forM_ ks $ \k -> do
    h <- lookup' h t k
    Just k @=? h
 where
   h = const 7

unit_lookup_nothing_conflict :: IO ()
unit_lookup_nothing_conflict = do
  let ks = ["A", "B", "C", "D"]
  t <- new
  mapM_ (\x -> insert' h t x x) ks
  h <- lookup' h t "X"
  h @=? Nothing
 where
   h = const 7

unit_lookup_nothing :: IO ()
unit_lookup_nothing = do
  let ks = ["A", "B", "C", "D"]
  t <- new
  mapM_ (\x -> insert t x x) ks
  h <- lookup t "X"
  h @=? Nothing

unit_grow_rehash :: IO ()
unit_grow_rehash = do
  let ks = ["A","Z", "C", "Y", "E", "X", "G", "W", "ab", "cd", "ef", "gh", "xx"]
  t <- newSized 8
  mapM_ (\k -> insert t k k) ks
  forM_ ks $ \k -> do
    h <- lookup t k
    Just k @=? h

unit_grow_rehash2 :: IO ()
unit_grow_rehash2 = do
  let ks = [0..99::Int]
  t <- newSized 8
  mapM_ (\k -> insert t k k) ks
  forM_ ks $ \k -> do
    h <- lookup t k
    Just k @=? h

unit_delete :: IO ()
unit_delete = do
  let ks = ["A","B", "C"]
  t <- new
  mapM_ (\k -> insert t k k) ks
  delete t "B"
  h <- lookup t "B"
  h @=? Nothing
  h <- lookup t "C"
  h @=? Just "C"
