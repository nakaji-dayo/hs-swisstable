module Data.HashTable.IO.Swiss where

import           Control.Monad.ST
import           Data.HashTable.ST.Swiss              as H
import Data.Hashable (Hashable)

new :: IO (Table RealWorld k v)
new = stToIO H.new
{-# INLINE new #-}

newSized :: (Int -> IO (Table RealWorld k v))
newSized = stToIO . H.newSized
{-# INLINE newSized #-}

insert :: (Hashable k, Eq k) => Table RealWorld k v -> k -> v -> IO ()
insert t k = stToIO . H.insert t k
{-# INLINE insert #-}

insert' :: (Hashable k, Eq k) => (k -> Int) -> Table RealWorld k v -> k -> v -> IO ()
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


getSize :: Table RealWorld k v -> IO Int
getSize  = stToIO . H.getSize
{-# INLINE getSize #-}

mutateST :: (Eq k, Hashable k)
         => Table RealWorld k v -> k -> (Maybe v -> ST RealWorld (Maybe v, a)) -> IO a
mutateST t k = stToIO . H.mutateST t k
{-# INLINE mutateST #-}

mutate :: (Eq k, Hashable k)
         => Table RealWorld k v -> k -> (Maybe v -> (Maybe v, a)) -> IO a
mutate t k = stToIO . H.mutate t k
{-# INLINE mutate #-}

mapM_ :: ((k, v) -> ST RealWorld a) -> Table RealWorld k v -> IO ()
mapM_ f = stToIO . H.mapM_ f

foldM :: (a -> (k,v) -> ST RealWorld a) -> a -> Table RealWorld k v -> IO a
foldM f i = stToIO . H.foldM f i
