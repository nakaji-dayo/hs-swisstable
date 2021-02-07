{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Lib where

import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr

import           Data.Char                (intToDigit)
import           Numeric                  (showHex, showIntAtBase)

import           Control.Monad
import           Data.Bits
import           Data.Hashable

-- import qualified Data.Vector           as V
-- import           Data.Vector.Mutable   (MVector)
-- import qualified Data.Vector.Mutable   as V

-- import qualified Data.Vector           as V
-- import           Data.Vector.Mutable   (MVector)
-- import qualified Data.Vector.Mutable   as V
import           Control.Exception        (assert)
import           Control.Monad.Primitive  (PrimMonad (PrimState), RealWorld, liftPrim)
import           Data.Maybe               (listToMaybe, mapMaybe)
import           Data.Primitive
import           Data.Primitive.Array     as A
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr       as PP
import           Debug.Trace
import GHC.Stack (HasCallStack)
import Data.STRef

foreign import ccall "_elm_cmp_vec" cElmCmpVec :: Word8 -> Ptr Word8 -> Word32
foreign import ccall "_load_movemask" cLoadMovemask :: Ptr Word8 -> Word32
foreign import ccall "ffs" cFfs :: Word32 -> Int
foreign import ccall "_elm_add_movemask" cElmAddMovemask :: Word8 -> Ptr Word8 -> Word32

showB :: Word32 -> String
showB x = showIntAtBase 2 intToDigit x ""

f = do
  src <- mallocArray 32
  pokeArray src [0..31]
  let res = cElmCmpVec 5 src
  putStrLn $ showB res

-- todo: distibute STRef
data Table s k v = Table
 { elems :: STRef s (MutableArray s (k, v))
 , ctrl  :: STRef s (MutablePrimArray s Word8)
 , size  :: STRef s Int
 , used  :: STRef s Int
 }

new :: PrimMonad m => m (Table (PrimState m) k v)
new = newSized 8

newSized :: HasCallStack => PrimMonad m => Int -> m (Table (PrimState m) k v)
newSized n = do
  when (n .&. (n - 1) /= 0) $ error "size should be power of 2"
  es <- A.newArray n (error "impossible access")
  c <- newPinnedPrimArray (n + 32)
  setPrimArray c 0 n 128
  setPrimArray c n 32 254
  ses <- liftPrim $ newSTRef es
  sc <- liftPrim $ newSTRef c
  su <- liftPrim $ newSTRef 0
  ss <- liftPrim $ newSTRef (fromIntegral n)
  pure $ Table ses sc ss su

insert' :: (PrimMonad m, Hashable k) => (k -> Int) -> k -> v -> Table (PrimState m) k v -> m ()
insert' hash' k v m = do
  let h1' = h1 hash' k
  s <- liftPrim $ readSTRef  (size m)
  findAndWrite ((s - 1) .&. h1') False
  liftPrim $ modifySTRef (used m) (+1)
  checkOverflow m >>= \x -> when x $ grow m
  where
    findAndWrite idx sndt = do -- todo rm s
      s <- liftPrim $ readSTRef  (size m)
      es <- liftPrim $ readSTRef (elems m)
      ct <- liftPrim $ readSTRef (ctrl m)
      let pc = PP.advancePtr (mutablePrimArrayContents ct) idx
      let mask = cLoadMovemask pc
      let offset = cFfs mask - 1
      let idx' = idx + offset
      if offset < 0 || idx' >= s then assert (not sndt) $ findAndWrite 0 True else (do
        writeArray es idx' (k, v)
        writePrimArray ct idx' (h2 hash' k))

lookup' :: (PrimMonad m, Hashable k, Show k, Eq k)
  => (k -> Int) -> k -> Table (PrimState m) k v -> m (Maybe v)
lookup' hash' k m = do
  s <- liftPrim $ readSTRef  (size m)
  let h1' = h1 hash' k
      h2' = h2 hash' k
  let idx = (s - 1) .&. h1'
  es <- liftPrim $ readSTRef $ elems m
  ct <- liftPrim $ readSTRef (ctrl m)

  let f'' idx _ = do
        let pc = PP.advancePtr (mutablePrimArrayContents ct) idx
        let mask = cElmCmpVec h2' pc
        -- recursive with bitmask
        foldFirstBitmask (readBM es idx) mask >>= \case
          Nothing
            | cElmCmpVec 128 pc /= 0 -> pure (Just Nothing) -- found empty
            | otherwise -> pure Nothing
          x       -> pure (Just x)
  iterateCtrlIdx f'' s idx
  where
    readBM es idx bidx = do
      let idx' = idx + bidx - 1
      (k', v) <- readArray es idx'
      pure $ if k == k'
             then Just v
             else Nothing

iterateCtrlIdx :: Monad m => (Int -> Bool -> m (Maybe b)) -> Int -> Int -> m b
iterateCtrlIdx f s idx = do
  go idx False
  where
    go idx sndt = do
      let next = idx + 32
          islast = next > s
      r <- f idx islast
      case r of
        Nothing -> do
          if islast then assert (not sndt) (go 0 True) else go next sndt
        Just x -> pure x

foldFirstBitmask :: Monad m => (Int -> m (Maybe a)) -> Word32 -> m (Maybe a)
foldFirstBitmask  f mask = do
  let bitidxs = map cFfs $ iterate (\x -> x .&. (x - 1)) mask
  go bitidxs
  where
    go (bidx:bidxs)
      | bidx == 0 = pure Nothing
      | otherwise = do
          r <- f bidx
          case r of
            Nothing -> go bidxs
            x -> pure x

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

h1 :: Hashable k => (k -> Int) -> k -> Int
h1 = ($)

h2 :: Hashable a => (a -> Int) -> a -> Word8
h2 h x = fromIntegral $ h x .&. 127

delete :: (PrimMonad m, Hashable k, Eq k) => k -> Table (PrimState m) k v -> m ()
delete = delete' hash

delete' :: (PrimMonad m, Hashable k, Eq k) => (k -> Int) -> k -> Table (PrimState m) k v -> m ()
delete' hash' k m= do
  s <- liftPrim $ readSTRef  (size m)
  let h1' = h1 hash' k
      h2' = h2 hash' k
  let idx = (s - 1) .&. h1'
  es <- liftPrim $ readSTRef $ elems m
  ct <- liftPrim $ readSTRef (ctrl m)
  let f'' idx _ = do
        let pc = PP.advancePtr (mutablePrimArrayContents ct) idx
        let mask = cElmCmpVec h2' pc
        foldFirstBitmask (readBM es idx) mask >>= \case
          Nothing
            | cElmCmpVec 128 pc /= 0 -> pure (Just Nothing)
            | otherwise -> pure Nothing
          x       -> pure (Just x)
  idx' <- iterateCtrlIdx f'' s idx
  forM_ idx' $ \idx' -> writePrimArray ct idx' 254
  where
    readBM es idx bidx = do
      let idx' = idx + bidx - 1
      (k', v) <- readArray es idx'
      pure $ if k == k'
             then Just idx'
             else Nothing


insert :: (PrimMonad m, Hashable k) => k -> v -> Table (PrimState m) k v -> m ()
insert = insert' hash

lookup :: (PrimMonad m, Hashable k, Show k, Eq k)
  => k -> Table (PrimState m) k v -> m (Maybe v)
lookup = lookup' hash

checkOverflow ::
  (PrimMonad m, Hashable k) => Table (PrimState m) k v -> m Bool
checkOverflow t@Table{..} = do
  u <- liftPrim $ readSTRef used
  s <- liftPrim $ readSTRef size
  pure $ fromIntegral u / fromIntegral s > 0.8

grow :: (PrimMonad m, Hashable k) => Table (PrimState m) k v -> m ()
grow t = do
  s <- liftPrim $ readSTRef  (size t)
  let size' = s * 2
  traceShowM ("grow", size')
  t' <- newSized size'
  mapM_' (f t') t
  elems' <- liftPrim $ readSTRef (elems t')
  liftPrim $ writeSTRef (elems t) elems'
  ctrl' <- liftPrim $ readSTRef (ctrl t')
  liftPrim $ writeSTRef (ctrl t) ctrl'
  size' <- liftPrim $ readSTRef (size t')
  liftPrim $ writeSTRef (size t) size'
  pure ()
  where
    f t (k, v) = insert k v t

mapM_' :: (PrimMonad m, Hashable k) => ((k, v) -> m a)
       -> Table (PrimState m) k v -> m ()
mapM_' f t = do
  let idx = 0
  ct <- liftPrim $ readSTRef (ctrl t)
  s <- liftPrim $ readSTRef (size t)
  iterateCtrlIdx (h ct s) s idx
  pure ()
  where
    g idx bidx = do
      es <- liftPrim $ readSTRef (elems t)
      let idx' = idx + bidx - 1
      e <- readArray es idx'
      f e
      pure Nothing
    h ct s idx islast = do
      let pc = PP.advancePtr (mutablePrimArrayContents ct) idx
      let mask = cElmAddMovemask 128 pc
      r <- foldFirstBitmask (g idx) mask
      if islast then pure (Just Nothing) else pure r
{-
試したい
　右端で競合が発生した際に0に戻るのではなく、
　予備領域を使い、予備領域が埋まったら拡張する。
-}
