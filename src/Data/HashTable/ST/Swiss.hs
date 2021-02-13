{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.HashTable.ST.Swiss
  ( Table (..)
  , new
  , newSized
  , insert'
  , insert
  , lookup'
  , lookup
  , delete'
  , delete
  , foldM
  , mapM_
  , analyze
  , getSize
  , mutateST
  , mutate
  ) where

import           Control.DeepSeq         (NFData)
import           Control.Exception       (assert)
import           Control.Monad           (forM_, void, when)
import qualified Control.Monad           as M
import           Control.Monad.Primitive (liftPrim)
import           Control.Monad.ST        (RealWorld, ST)
import           Data.Bits
import           Data.Hashable
import           Data.Primitive
import           Data.Primitive.Array    as A
import           Data.Primitive.Ptr      as PP
import           Data.STRef
import           Data.Word
import           Foreign.C.Types
import           GHC.Generics            (Generic)
import           GHC.IO                  (ioToST)
import           Prelude                 hiding (lookup, mapM_)

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
new = newSized 16

empty :: Word8
empty = 128

deleted :: Word8
deleted = 254

newSized :: Int -> ST s (Table s k v)
newSized n = do
  when (n .&. (n - 1) /= 0) $ error "size should be power of 2"
  es <- A.newArray n (error "impossible")
  c <- newPinnedPrimArray (n + 32)
  setPrimArray c 0 n empty
  setPrimArray c n 32 deleted
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
insert' :: (Hashable k, Eq k) => (k -> Int) -> Table s k v -> k -> v -> ST s ()
insert' hash' ref k v = do
  delete' hash' ref k
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
    f m idx = do
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


lookup' :: forall k s a. (Hashable k, Eq k) => (k -> Int) -> Table s k a -> k -> ST s (Maybe a)
lookup' hash' ref !k = do
  Table{..} <- readRef ref
  let !idx = mask .&. h1'
  iterateCtrlIdx (lookCtrlAt (mutablePrimArrayContents ctrl) elems) size idx
  where
    !h1' = h1 hash' k
    !h2' = h2 h1'
    -- lookBitmask' :: MutableArray s (k, a) -> Int -> Int -> ST s (Maybe (Maybe a))
    -- lookBitmask' es idx bidx
    --   | bidx == 0 = pure $ Just Nothing
    --   | otherwise = do
    --       let idx' = idx + bidx - 1
    --       (!k', v) <- readArray es idx'
    --       pure $ if k == k' -- todo: opt(hashも保持？)
    --              then Just (Just v)
    --              else Nothing
    lookBitmask es idx bidx = do
      let idx' = idx + bidx - 1
      (!k', v) <- readArray es idx'
      pure $ if k == k' -- todo: opt(hashも保持？)
             then Just v
             else Nothing
    {-# INLINE lookBitmask #-}
    lookCtrlAt !ptr !es !idx = do
        let pc = PP.advancePtr ptr idx
        let !mask = cElmCmpVec h2' pc
        -- recursive with bitmask
        -- Just x <- firstJustM (lookBitmask' es idx) (listBitmaskSet mask)
        x <- iterateBitmaskSet (lookBitmask es idx) mask
        case x of
          Nothing
            | cElmCmpVec 128 pc /= 0 -> pure (Just Nothing) -- found empty -- unlikely
            | otherwise -> pure Nothing
          _       -> pure (Just x)
    {-# INLINE lookCtrlAt #-}
{-# INLINE lookup' #-}

iterateCtrlIdx :: Monad m => (Int -> m (Maybe b)) -> Int -> Int -> m b
iterateCtrlIdx f !s !offset = go offset
  where
    go !idx = do
      f idx >>= \case
        Nothing ->
          let !next = idx + 32
          in if next > s then go 0 else go next
        Just x -> pure x
{-# INLINE iterateCtrlIdx #-}

listBitmaskSet :: Word32 -> [CInt]
listBitmaskSet = map cFfs . iterate (\x -> x .&. (x - 1))
{-# INLINE listBitmaskSet #-}

iterateBitmaskSet :: Monad m => (Int -> m (Maybe a)) -> Word32 -> m (Maybe a)
iterateBitmaskSet  !f !mask = do
  let bitidxs = listBitmaskSet mask
  go bitidxs
  where
    go (bidx:bidxs)
      | bidx /= 0 = do
          f (fromIntegral bidx) >>= \case
            Nothing -> go bidxs
            x       -> pure x
      | otherwise = pure Nothing
    go _ = pure Nothing
    {-# INLINE go #-}
{-# INLINE iterateBitmaskSet #-}

{- I want to use the infinite list, but I can't speed it up. -}
-- listBitmaskSet :: Word32 -> [Int]
-- listBitmaskSet !mask = map (fromIntegral . cFfs) $ iterate (\x -> x .&. (x - 1)) mask
-- {-# INLINE listBitmaskSet #-}

-- firstJustM f (x:xs) = f x >>= \case
--   Just r -> pure $ Just r
--   Nothing -> firstJustM f xs
-- firstJustM f _ = pure Nothing
-- {-# INLINE firstJustM #-}

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
  let f'' offset = do
        let pc = PP.advancePtr (mutablePrimArrayContents ct) offset
        let mask = cElmCmpVec h2' pc
        iterateBitmaskSet (readBM es offset) mask >>= \case
          Nothing
            | cElmCmpVec 128 pc /= 0 -> pure (Just Nothing)
            | otherwise -> pure Nothing
          x       -> pure (Just x)
  idx' <- iterateCtrlIdx f'' s idx
  forM_ idx' $ \x -> do
    writePrimArray ct x 254
    let m' = m {used = used m - 1}
    liftPrim $ writeRef ref m'
  where
    readBM es offset bidx = do
      let idx' = offset + bidx - 1
      (k', _) <- readArray es idx'
      pure $ if k == k'
             then Just idx'
             else Nothing


-- insert :: (PrimMonad m, Hashable k) => k -> v -> Table (PrimState m) k v -> m ()
insert :: (Hashable k, Eq k) => Table s k v -> k -> v -> ST s ()
insert = insert' hash

-- lookup :: (PrimMonad m, Hashable k, Show k, Eq k)
--   => k -> Table (PrimState m) k v -> m (Maybe v)
lookup :: (Hashable k, Eq k) => Table s k a -> k -> ST s (Maybe a)
lookup = lookup' hash
{-# INLINE lookup #-}

checkOverflow ::
  (Hashable k) => Table s k v -> ST s Bool
checkOverflow ref = do
  t <- readRef ref
  pure $ fromIntegral (used t) / fromIntegral (size t) > maxLoad

maxLoad = 0.8 :: Double

grow :: (Hashable k, Eq k) => Table s k v -> ST s ()
grow ref = do
  t <- readRef ref
  let size' = size t * 2
  t' <- newSized size'
  mapM_ (f t') ref
  writeRef ref =<< readRef t'
  pure ()
  where
    f t (k, v) = insert t k v

mapM_ :: ((k, v) -> ST s a) -> Table s k v -> ST s ()
mapM_ f ref = do
  t <- readRef ref
  let idx = 0
  void $ iterateCtrlIdx (h t) (size t) idx
  where
    g t idx bidx = do
      let idx' = idx + bidx - 1
      e <- readArray (elems t) idx'
      void $ f e
      pure Nothing
    h t idx = do
      let pc = PP.advancePtr (mutablePrimArrayContents (ctrl t)) idx
      let mask = cElmAddMovemask 128 pc
      r <- iterateBitmaskSet (g t idx) mask
      if idx + 32 > size t then pure (Just Nothing) else pure r

foldM :: (a -> (k,v) -> ST s a) -> a -> Table s k v -> ST s a
foldM f seed0 ref = do
  t <- readRef ref
  foldCtrlM g seed0 t 0
  where
    g acc t idx (bidx:xs)
      | bidx == 0 = pure acc
      | otherwise = do
          let idx' = idx + bidx - 1
          e <- readArray (elems t) idx'
          acc' <- f acc e
          g acc' t idx xs
    g _ _ _ _ = error "impossible"

foldCtrlM :: (a -> Table_ s k v -> Int -> [Int] -> ST s a) -> a -> Table_ s k v -> Int -> ST s a
foldCtrlM g acc t idx = do
  let pc = PP.advancePtr (mutablePrimArrayContents (ctrl t)) idx
  let mask = cElmAddMovemask 128 pc
  acc' <- g acc t idx (map fromIntegral $ listBitmaskSet mask)
  if idx + 32 > size t then pure acc' else foldCtrlM g acc' t (idx + 32)

_foldM :: (a -> (k,v) -> Int -> ST s a) -> a -> Table s k v -> ST s a
_foldM f seed0 ref = do
  t <- readRef ref
  foldCtrlM g seed0 t 0
  where
    g acc t idx (bidx:xs)
      | bidx == 0 = pure acc
      | otherwise = do
          let idx' = idx + bidx - 1
          e <- readArray (elems t) idx'
          acc' <- f acc e idx'
          g acc' t idx xs
    g _ _ _ _ = error "impossible"

analyze :: (Hashable k, Show k) => (Table RealWorld k v -> ST RealWorld ())
analyze ref = do
  t <- readRef ref
  cs <- _foldM (f t) [] ref
  ioToST $ do
    putStrLn $ "size: " <> show (size t)
    putStrLn $ "used: " <> show (used t)
    putStrLn $ "  " <> show (fromIntegral (used t) / fromIntegral (size t) :: Double)
    print $ "max diff: " <> show (maximum (fmap snd cs))
    print $ "sum diff: " <> show (sum (fmap snd cs))
    M.mapM_ print cs
  where
    f t acc (k, _) idx = do
      let nidx = (size t - 1) .&. hash k
      let d = if idx - nidx < 0 then idx - nidx + size t else idx - nidx
      pure $ ((k, nidx, idx), d):acc

mutateST :: (Eq k, Hashable k)
         => Table s k v -> k -> (Maybe v -> ST s (Maybe v, a)) -> ST s a
mutateST ref k f = do
  v <- lookup ref k
  -- todo: opt: lookup single time
  f v >>= \case
    (Just v', a) -> do
      insert ref k v'
      pure a
    (Nothing, a) -> do
      delete ref k
      pure a

mutate :: (Eq k, Hashable k) =>
  Table s k v -> k -> (Maybe v -> (Maybe v, a)) -> ST s a
mutate ref !k !f = mutateST ref k (pure . f)

{-
試したい
　右端で競合が発生した際に0に戻るのではなく、
　予備領域を使い、予備領域が埋まったら拡張する。
  -> unlikelyすぎて効果うすそう
-- make Data.HashTable.Class instance?
  -> 両方に依存したインターフェス揃えるようlibrary作れば良い
-}

getSize :: Table s k v -> ST s Int
getSize  = fmap size . readRef
