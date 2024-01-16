{-# LANGUAGE BlockArguments #-}

module HW6.T1
  ( BucketsArray,
    CHT (..),
    newCHT,
    getCHT,
    putCHT,
    sizeCHT,
    initCapacity,
    loadFactor,
  )
where

import Control.Concurrent.Classy (MonadConc (atomically), MonadSTM (newTVar, writeTVar), STM, readTVar)
import Control.Concurrent.Classy.STM (TArray, TVar)
import Control.Monad (forM_, unless, when)
import Data.Array.Base (MArray (getNumElements), getElems, newArray, readArray, writeArray)
import Data.Hashable
import Data.List (find)
import Data.Maybe (isNothing)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]

type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v),
    chtSize :: TVar stm Int
  }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically do
  bucketsArray <- newArray (0, initCapacity - 1) []
  bucketsTVar <- newTVar bucketsArray
  sizeTVar <- newTVar 0
  return $ CHT bucketsTVar sizeTVar

getCHT ::
  ( MonadConc m,
    Eq k,
    Hashable k
  ) =>
  k ->
  CHT (STM m) k v ->
  m (Maybe v)
getCHT key table = atomically do
  arr <- readTVar $ chtBuckets table
  cap <- getNumElements arr
  curArr <- readArray arr $ hash key `mod` cap
  return $ snd <$> find (\(k, _) -> k == key) curArr

putCHT ::
  ( MonadConc m,
    Eq k,
    Hashable k
  ) =>
  k ->
  v ->
  CHT (STM m) k v ->
  m ()
putCHT key val table = atomically do
  arr <- readTVar $ chtBuckets table
  sz <- readTVar $ chtSize table
  cap <- getNumElements arr
  let ind = hash key `mod` cap
  curArr <- readArray arr ind
  let notFound = isNothing $ find (\(k, _) -> k == key) curArr
  let shouldResize = not notFound && fromIntegral sz + 1 >= loadFactor * fromIntegral cap
  when notFound do
    unless shouldResize do
      writeArray arr ind ((key, val) : curArr)
      writeTVar (chtBuckets table) arr
    when shouldResize do
      newArr <- newArray (0, 2 * cap - 1) []
      els <- getElems arr
      forM_ (concat els) $ \(k, v) -> do
        let newInd = hash k `mod` (2 * cap)
        interBucket <- readArray newArr newInd
        writeArray newArr newInd ((k, v) : interBucket)
      writeTVar (chtBuckets table) newArr
    writeTVar (chtSize table) $ sz + 1
  unless notFound do
    writeArray arr ind ((key, val) : filter (\(k, _) -> k /= key) curArr)
    writeTVar (chtBuckets table) arr

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT m = atomically do
  readTVar $ chtSize m
