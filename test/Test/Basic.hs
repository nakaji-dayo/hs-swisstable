module Test.Basic where

import           Prelude                  hiding (lookup)

import           Control.Monad
import           Data.HashTable.IO.Swiss
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
  forM_ ks $ \k -> do
    h <- lookup ref k
    Just k @=? h

unit_insertAndLookup_rand :: IO ()
unit_insertAndLookup_rand = do
  ks <- generate (vector 1000 :: Gen [Int])
  ref <- new
  mapM_ (\k -> insert ref k k) ks
  forM_ ks $ \k -> do
    h <- lookup ref k
    Just k @=? h

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

-- 一旦deleteでごまかす
-- vのswapできるようにすべき
unit_update :: IO ()
unit_update = do
  let ks = take 5 $ repeat "A"
  t <- newSized 4
  mapM_ (\x -> insert t x x) ks
  s <- getSize t
  4 @=? s

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

unit_foldM :: IO ()
unit_foldM = do
  let ks = ["A","B", "C"]
  t <- new
  mapM_ (\k -> insert t k k) ks
  x <- stToIO $ foldM' (\acc (k, _) -> pure (acc ++ k)) "" t
  "ABC" @=? x

unit_mutate :: IO ()
unit_mutate = do
  let ks = ["A","B", "C"]
  t <- new
  mapM_ (\k -> insert t k k) ks
  mutate t "A" (\(Just v) -> (Just (v ++ "!"), ()))
  mutate t "B" (const (Nothing, ()))
  a <- lookup t "A"
  Just "A!" @=? a
  a <- lookup t "B"
  Nothing @=? a
