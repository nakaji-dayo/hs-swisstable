module IO where

import           Control.Monad.ST
import           Lib              as H
import Data.Hashable (Hashable)

new :: IO (Table RealWorld k v)
new = stToIO H.new
{-# INLINE new #-}

newSized :: (Int -> IO (Table RealWorld k v))
newSized = stToIO . H.newSized
{-# INLINE newSized #-}

insert :: (Hashable k => Table RealWorld k v -> k -> v -> IO ())
insert t k = stToIO . H.insert t k
{-# INLINE insert #-}

insert' :: (Hashable k => (k -> Int) -> Table RealWorld k v -> k -> v -> IO ())
insert' h t k = stToIO . H.insert' h t k
{-# INLINE insert' #-}

lookup :: ((Hashable k, Show k, Eq k) => Table RealWorld k a -> k -> IO (Maybe a))
lookup t = stToIO . H.lookup t
{-# INLINE lookup #-}

lookup' :: ((Hashable k, Show k, Eq k) => (k -> Int)
                  -> Table RealWorld k a -> k -> IO (Maybe a))
lookup' h t = stToIO . H.lookup' h t
{-# INLINE lookup' #-}

delete :: ((Hashable k, Show k, Eq k) => Table RealWorld k v -> k -> IO ())
delete t = stToIO . H.delete t
{-# INLINE delete #-}
