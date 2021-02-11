{-# LANGUAGE BangPatterns    #-}
module Main where

import           Control.DeepSeq
import           Control.Monad
import           Criterion
import           Criterion.Main
import qualified Data.HashTable.IO       as H
import qualified Data.HashTable.ST.Basic
import           Data.Maybe
import           IO
import           Prelude                 hiding (lookup)
import           Test.QuickCheck         (Gen, generate, vector)

smallKeys = [1..10000::Int]

initSmall new insert = do
  t <- new
  mapM_ (\x -> insert t x x) smallKeys
  pure $! t


benchSmall name new insert lookup =
  env (initSmall new insert) $ \t -> bench name $ whnfIO $ do
  mapM_ (lookup t) smallKeys


initSmallRand new insert = do
  t <- new
  ks <- generate (vector 10000 :: Gen [Int])
  mapM_ (\x -> insert t x x) ks
  pure (ks, t)

benchSmallRand name new insert lookup =
  env (initSmallRand new insert) $ \ ~(ks, t) -> bench name $ whnfIO $ do
  mapM_ (lookup t) ks

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
      [ bgroup
      "small"
        [ benchSmallRand "SwissTable" new insert lookup -- 衝突凄い。多分バグ
        , benchSmallRand "BasicHashTable" (H.new :: IO (H.BasicHashTable Int Int)) H.insert H.lookup
        ]
      ]

      -- bgroup
      --   "insert(seq)"
      --   [ bgroup
      --       "small"
      --       [ bench "swiss" $
      --           whnfIO $ do
      --             t <- newSized (2 ^ 16)
      --             mapM_ (\x -> insert x x t) smallKeys
      --       , bench "HashTable.IO" $
      --           whnfIO $ do
      --             t <- H.new :: IO (H.BasicHashTable Int Int)
      --             mapM_ (\x -> H.insert t x x) smallKeys
      --       ]
      --   ]
    ]

instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()
