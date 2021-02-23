module Main where

import           Control.DeepSeq
import qualified Data.HashTable.IO       as H
import           Data.HashTable.IO.Swiss hiding (fildM, mapM_)
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Swiss as S
import           Weigh

ssize = 1000000

main =
  mainWith $ do
    io "Swiss.insert" (testInsert new insert) ssize
    io "Data.HashTable.ST.Basic.insert" (testInsert (H.new :: IO (H.BasicHashTable Int Int)) H.insert) ssize
    io "Swiss.insert sized" (testInsert (newSized (2^21)) insert) ssize
    io "Data.HashTable.ST.Basic.insert sized" (testInsert (H.newSized (2^21) :: IO (H.BasicHashTable Int Int)) H.insert) ssize

testInsert new insert size = do
  t <- new
  mapM_ (\x -> insert t x x) ([1..size] :: [Int])
  pure t

instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()
instance NFData (S.Table s k v)
