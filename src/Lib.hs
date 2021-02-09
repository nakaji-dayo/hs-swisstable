{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( Table (..)
  , new
  , newSized
  , insert'
  , insert
  , lookup'
  , lookup
  , delete'
  , delete
  ) where

import           Control.DeepSeq         (NFData)
import           Control.Exception       (assert)
import           Control.Monad
import           Control.Monad.Primitive (liftPrim)
import           Control.Monad.ST        (ST)
import           Data.Bits
import           Data.Hashable
import           Data.Primitive
import           Data.Primitive.Array    as A
import           Data.Primitive.Ptr      as PP
import           Data.STRef
import           Data.Word
import           Debug.Trace
import           Foreign.C.Types
import           GHC.Generics            (Generic)
import           Prelude                 hiding (lookup)

-- todo: try foreign import prim
foreign import ccall unsafe "_elm_cmp_vec" cElmCmpVec :: Word8 -> Ptr Word8 -> Word32
foreign import ccall unsafe "_load_movemask" cLoadMovemask :: Ptr Word8 -> Word32
foreign import ccall unsafe "ffs" cFfs :: Word32 -> CInt
foreign import ccall unsafe "_elm_add_movemask" cElmAddMovemask :: Word8 -> Ptr Word8 -> Word32

newtype Table s k v = T (STRef s (Table_ s k v))
  deriving (Generic, NFData)

-- todo: distibute STRef
data Table_ s k v = Table
 { elems ::  {-# UNPACK #-} !(MutableArray s (k, v))
 , ctrl  ::  {-# UNPACK #-} !(MutablePrimArray s Word8)
 , size  ::  {-# UNPACK #-} !Int
 , mask  ::  {-# UNPACK #-} !Int
 , used  ::  {-# UNPACK #-} !Int
 } deriving (Generic)

new :: ST s (Table s k v)
new = newSized 8

newSized :: Int -> ST s (Table s k v)
newSized n = do
  when (n .&. (n - 1) /= 0) $ error "size should be power of 2"
  es <- A.newArray n (error "impossible")
  c <- newPinnedPrimArray (n + 32)
  setPrimArray c 0 n 128
  setPrimArray c n 32 254
  let t = Table es c (fromIntegral n) (fromIntegral n - 1) 0
  newRef t

newRef :: Table_ s k v -> ST s (Table s k v)
newRef = fmap T . newSTRef
{-# INLINE newRef #-}

readRef :: Table s k v -> ST s (Table_ s k v)
readRef (T ref) = readSTRef ref
{-# INLINE readRef #-}

writeRef :: Table s k v -> Table_ s k v -> ST s ()
writeRef (T ref) = writeSTRef ref
{-# INLINE writeRef #-}

-- insert' :: (PrimMonad m, Hashable k) => (k -> Int) -> k -> v -> Table (PrimState m) k v -> m ()
insert' :: (Hashable k) => (k -> Int) -> Table s k v -> k -> v -> ST s ()
insert' hash' ref k v = do
  m <- readRef ref
  let s = size m
  iterateCtrlIdx (f m) (size m) ((s - 1) .&. h1')
  -- todo: cost?
  let m' = m {used = used m + 1}
  liftPrim $ writeRef ref m'
  checkOverflow ref >>= \x -> when x $ grow ref
  where
    -- findAndWrite m idx sndt = do
    --   let ct = ctrl m
    --   let pc = PP.advancePtr (mutablePrimArrayContents ct) idx
    --   let mask = cLoadMovemask pc
    --   let offset = cFfs mask - 1
    --   let idx' = idx + fromIntegral offset
    --   traceShowM (idx, idx', offset)
    --   if offset < 0 || idx' >= size m
    --     then assert (not sndt) $ findAndWrite m 0 True
    --     else (do
    --              writeArray (elems m) idx' (k, v)
    --              writePrimArray ct idx' (h2 hash' k))
    !h1' = h1 hash' k
    f m idx _ = do
      let ct = ctrl m
      let pc = PP.advancePtr (mutablePrimArrayContents ct) idx
      let mask = cLoadMovemask pc
      let offset = cFfs mask - 1
      let idx' = idx + fromIntegral offset
      if offset < 0 || idx' >= size m then pure Nothing
        else (do
                 writeArray (elems m) idx' (k, v)
                 writePrimArray ct idx' (h2 h1')
                 pure $ Just ()
             )


lookup' :: (Hashable k, Show k, Eq k) => (k -> Int) -> Table s k a -> k -> ST s (Maybe a)
lookup' hash' ref !k = do
  Table{..} <- readRef ref
  let !idx = mask .&. h1'
  iterateCtrlIdx (lookCtrlAt (mutablePrimArrayContents ctrl) elems) size idx
  where
    !h1' = h1 hash' k
    !h2' = h2 h1'
    lookBitmask es idx bidx = do
      let idx' = idx + bidx - 1
      (k', v) <- readArray es idx'
      pure $ if k == k' -- todo: opt(hashも保持？)
             then Just v
             else Nothing
    {-# INLINE lookBitmask #-}
    lookCtrlAt ptr es idx _ = do
        let pc = PP.advancePtr ptr idx
        let mask = cElmCmpVec h2' pc
        -- recursive with bitmask
        foldFirstBitmask (lookBitmask es idx) mask >>= \case
          Nothing
            | cElmCmpVec 128 pc /= 0 -> pure (Just Nothing) -- found empty -- unlikely
            | otherwise -> pure Nothing
          x       -> pure (Just x)
    {-# INLINE lookCtrlAt #-}
{-# INLINE lookup' #-}

iterateCtrlIdx :: Monad m => (Int -> Bool -> m (Maybe b)) -> Int -> Int -> m b
iterateCtrlIdx f s offset =
  go offset False
  where
    go !idx !sndt = do
      let next = idx + 32
          islast = next > s
      r <- f idx islast
      case r of
        Nothing ->
          if islast then assert (not sndt) (go 0 True) else go next sndt
        Just x -> pure x
{-# INLINE iterateCtrlIdx #-}

foldFirstBitmask :: Monad m => (Int -> m (Maybe a)) -> Word32 -> m (Maybe a)
foldFirstBitmask  !f !mask = do
  let bitidxs = map cFfs $ iterate (\x -> x .&. (x - 1)) mask
  go bitidxs
  where
    go (bidx:bidxs)
      | bidx == 0 = pure Nothing
      | otherwise = do
          r <- f (fromIntegral bidx)
          case r of
            Nothing -> go bidxs
            x       -> pure x
    go _ = pure Nothing
{-# INLINE foldFirstBitmask #-}

-- firstJust :: (a -> Maybe b) -> [a] -> Maybe b
-- firstJust f = listToMaybe . mapMaybe f

h1 :: Hashable k => (k -> Int) -> k -> Int
h1 = ($)
{-# INLINE h1 #-}

h2 :: Int -> Word8
h2 x = fromIntegral $ x .&. 127
{-# INLINE h2 #-}

-- delete :: (PrimMonad m, Hashable k, Eq k) => k -> Table (PrimState m) k v -> m ()
delete :: (Hashable k, Eq k) => Table s k v -> k -> ST s ()
delete = delete' hash

-- delete' :: (PrimMonad m, Hashable k, Eq k) => (k -> Int) -> k -> Table (PrimState m) k v -> m ()
delete' :: (Hashable k, Eq k) => (k -> Int) -> Table s k v -> k -> ST s ()
delete' hash' ref k = do
  m <- readRef ref
  let s = size m
  let h1' = h1 hash' k
      h2' = h2 h1'
  let idx = (s - 1) .&. h1'
  let es = elems m
  let ct = ctrl m
  let f'' offset _ = do
        let pc = PP.advancePtr (mutablePrimArrayContents ct) offset
        let mask = cElmCmpVec h2' pc
        foldFirstBitmask (readBM es offset) mask >>= \case
          Nothing
            | cElmCmpVec 128 pc /= 0 -> pure (Just Nothing)
            | otherwise -> pure Nothing
          x       -> pure (Just x)
  idx' <- iterateCtrlIdx f'' s idx
  forM_ idx' $ \x -> writePrimArray ct x 254
  where
    readBM es offset bidx = do
      let idx' = offset + bidx - 1
      (k', _) <- readArray es idx'
      pure $ if k == k'
             then Just idx'
             else Nothing


-- insert :: (PrimMonad m, Hashable k) => k -> v -> Table (PrimState m) k v -> m ()
insert :: (Hashable k) => Table s k v -> k -> v -> ST s ()
insert = insert' hash

-- lookup :: (PrimMonad m, Hashable k, Show k, Eq k)
--   => k -> Table (PrimState m) k v -> m (Maybe v)
lookup :: (Hashable k, Show k, Eq k) => Table s k a -> k -> ST s (Maybe a)
lookup = lookup' hash
{-# INLINE lookup #-}

checkOverflow ::
  (Hashable k) => Table s k v -> ST s Bool
checkOverflow ref = do
  t <- readRef ref
  pure $ fromIntegral (used t) / fromIntegral (size t) > (0.8 :: Double)

grow :: (Hashable k) => Table s k v -> ST s ()
grow ref = do
  t <- readRef ref
  let size' = size t * 2
  t' <- newSized size'
  mapM_' (f t') ref
  writeRef ref =<< readRef t'
  pure ()
  where
    f t (k, v) = insert t k v

mapM_' :: ((k, v) -> ST s a) -> Table s k v -> ST s ()
mapM_' f ref = do
  t <- readRef ref
  let idx = 0
  void $ iterateCtrlIdx (h t) (size t) idx
  pure ()
  where
    g t idx bidx = do
      let idx' = idx + bidx - 1
      e <- readArray (elems t) idx'
      void $ f e
      pure Nothing
    h t idx islast = do
      let pc = PP.advancePtr (mutablePrimArrayContents (ctrl t)) idx
      let mask = cElmAddMovemask 128 pc
      r <- foldFirstBitmask (g t idx) mask
      if islast then pure (Just Nothing) else pure r

{-
試したい
　右端で競合が発生した際に0に戻るのではなく、
　予備領域を使い、予備領域が埋まったら拡張する。
  -> unlikelyすぎて効果うすそう
-- make Data.HashTable.Class instance?
  -> 両方に依存したインターフェス揃えるようlibrary作れば良い
-}
