{-# LANGUAGE BangPatterns    #-}
module Main where

import           Control.DeepSeq
import           Control.DeepSeq         (NFData)
import           Control.Monad
import           Criterion
import           Criterion.Main
import qualified Data.HashTable.IO       as H
import           Data.HashTable.IO.Swiss hiding (fildM, mapM_)
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Swiss as S
import           Data.Maybe
import           Prelude                 hiding (lookup)
import           Test.QuickCheck         (Gen, generate, vector)

smallKeys :: [Int]
smallKeys = [1..10000]

initSmall new insert = do
  t <- new
  mapM_ (\x -> insert t x x) smallKeys
  pure $! t


benchSmall name new insert lookup =
  env (initSmall new insert) $ \t -> bench name $ whnfIO $ do
  mapM_ (lookup t) smallKeys


genSmallRand = generate (vector 10000 :: Gen [Int])

initSmallRand new insert ks = do
  t <- new
  mapM_ (\x -> insert t x x) ks
  pure (ks, t)

benchSmallRand name new insert lookup ks =
  env (initSmallRand new insert ks) $ \ ~(ks, t) -> bench name $ whnfIO $ do
  mapM_ (lookup t) ks

insertSmallSeq name new insert =
  bench name $ whnfIO $ initSmall new insert

main =
  defaultMain
    [ bgroup
        "lookup(seq)"
        [ bgroup
            "small"
            [ benchSmall "SwissTable" new insert lookup
            , benchSmall "BasicHashTable" (H.new :: IO (H.BasicHashTable Int Int)) H.insert H.lookup
            ]
        ]
    , bgroup
      "lookup(rand)"
      [ env genSmallRand $ \k -> bgroup
      "small"
        [ benchSmallRand "SwissTable" new insert lookup k
        , benchSmallRand "BasicHashTable" (H.new :: IO (H.BasicHashTable Int Int)) H.insert H.lookup k
        ]
      ]
    , bgroup
        "insert(seq)"
        [ bgroup "sized"
          [ bgroup
            "small"
            [ insertSmallSeq "SwissTable" (newSized (2^16)) insert
            , insertSmallSeq "BasicHashTable" (H.newSized (2^16) :: IO (H.BasicHashTable Int Int)) H.insert
            ]
          ]
        ]
    ]

instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()
instance NFData (S.Table s k v)
