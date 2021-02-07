module Main where

import           Control.DeepSeq
import           Control.Monad
import           Criterion
import           Criterion.Main
import qualified Data.HashTable.IO       as H
import qualified Data.HashTable.ST.Basic
import           Data.Maybe
import           Lib
import           Prelude                 hiding (lookup)

smallKeys = [1..10000::Int]

main =
  defaultMain
    [ bgroup
        "lookup(seq)"
        [ bgroup
            "small"
            [ env (do
                      t <- newSized (2 ^ 16)
                      mapM_ (\x -> insert x x t) smallKeys
                      pure t
                  ) $ \t -> bench "swiss" $ whnfIO $ do
                mapM_ (\x -> lookup x t) smallKeys
            , env (do
                      t <- H.new :: IO (H.BasicHashTable Int Int)
                      mapM_ (\x -> H.insert t x x) smallKeys
                      pure t
                  ) $ \t -> bench "HashTable.IO" $ whnfIO $ do
                mapM_ (\x -> H.lookup t x) smallKeys
            ]
        ],
      bgroup
        "insert(seq)"
        [ bgroup
            "small"
            [ bench "swiss" $
                whnfIO $ do
                  t <- newSized (2 ^ 16)
                  mapM_ (\x -> insert x x t) smallKeys
            , bench "HashTable.IO" $
                whnfIO $ do
                  t <- H.new :: IO (H.BasicHashTable Int Int)
                  mapM_ (\x -> H.insert t x x) smallKeys
            ]
        ]
    ]

instance NFData (Data.HashTable.ST.Basic.HashTable s k v) where
  rnf x = seq x ()
