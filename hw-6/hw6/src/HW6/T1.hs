{-# HLINT ignore "Use readTVarIO" #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase   #-}

module HW6.T1
  ( BucketsArray
  , CHT(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , capacityCHT
  , initCapacity
  , loadFactor
  , getEntries
  ) where

import Control.Arrow (first)
import Control.Concurrent.Classy (MonadConc, STM, atomically, mask_)
import Control.Concurrent.Classy.STM (MonadSTM (TVar, newTVar, readTVar, writeTVar), TArray, modifyTVar')
import Control.Monad (forM_, when)
import Data.Array.Base
import Data.Hashable

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT =
  atomically $ do
    arr <- newBucketArray initCapacity >>= newTVar
    sizeVar <- newTVar 0
    return $ CHT {chtBuckets = arr, chtSize = sizeVar}

getCHT :: (MonadConc m, Eq k, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT !key cht =
  atomically $ getBucket key cht (\_ _ bucket -> return $ lookup key bucket)

putCHT :: (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT !key !value cht = mask_ $! insertCHT key value cht

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = atomically . sizeSTM

capacityCHT :: MonadConc m => CHT (STM m) k v -> m Int
capacityCHT = atomically . capacitySTM

getEntries :: MonadConc m => CHT (STM m) k v -> m (Bucket k v)
getEntries = atomically . entriesSTM

insertCHT ::
     (MonadConc m, Eq k, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
insertCHT !key !value cht@(CHT ar s) =
  atomically $! do
    arr <- readTVar ar
    sz <- readTVar s
    capacity <- getNumElements arr
    when (resizeRequired capacity sz) $! resizeCHT capacity arr cht
    tryInsertAndUpdateSize key value cht (+ 1)

resizeCHT ::
     (Eq k, Hashable k, MonadSTM stm)
  => Int
  -> BucketsArray stm k v
  -> CHT stm k v
  -> stm ()
resizeCHT capacity arr cht = do
  buckets <- mconcat <$> getElems arr
  newArr <- newBucketArray (capacity * 2)
  writeTVar (chtBuckets cht) newArr
  forM_ buckets (\(k, v) -> tryInsertAndUpdateSize k v cht id)

tryInsertAndUpdateSize ::
     (Eq k, Hashable k, MonadSTM stm)
  => k
  -> v
  -> CHT stm k v
  -> (Int -> Int)
  -> stm ()
tryInsertAndUpdateSize key value cht sizeFun =
  getBucket
    key
    cht
    (\idx bucketArr targetBucket -> do
       (newBucket, insertNew) <- replaceOrInsert key value targetBucket
       writeArray bucketArr idx newBucket
       modifyTVar'
         (chtSize cht)
         (if insertNew
            then sizeFun
            else id))

replaceOrInsert ::
     (Eq k, MonadSTM stm) => k -> v -> Bucket k v -> stm (Bucket k v, Bool)
replaceOrInsert !key value =
  \case
    [] -> return ([(key, value)], True)
    (kv@(x1, _):xs) ->
      if key == x1
        then return ((key, value) : xs, False)
        else first (kv :) <$> replaceOrInsert key value xs

getBucket ::
     (Hashable k, MonadSTM stm)
  => k
  -> CHT stm k v
  -> (Int -> BucketsArray stm k v -> Bucket k v -> stm a)
  -> stm a
getBucket !key cht callBack = do
  bucketArr <- bucketsSTM cht
  idx <- (hash key `mod`) <$> capacitySTM cht
  callBack idx bucketArr =<< readArray bucketArr idx

bucketsSTM :: MonadSTM stm => CHT stm k v -> stm (BucketsArray stm k v)
bucketsSTM cht = readTVar (chtBuckets cht)

sizeSTM :: MonadSTM stm => CHT stm k v -> stm Int
sizeSTM cht = readTVar (chtSize cht)

capacitySTM :: MonadSTM stm => CHT stm k v -> stm Int
capacitySTM cht = readTVar (chtBuckets cht) >>= getNumElements

newBucketArray :: MonadSTM stm => Int -> stm (BucketsArray stm k v)
newBucketArray !capacity = newArray (0, capacity - 1) []

entriesSTM :: MonadSTM stm => CHT stm k v -> stm (Bucket k v)
entriesSTM cht = bucketsSTM cht >>= fmap concat . getElems

resizeRequired :: Int -> Int -> Bool
resizeRequired cap sz = fromIntegral sz >= loadFactor * fromIntegral cap