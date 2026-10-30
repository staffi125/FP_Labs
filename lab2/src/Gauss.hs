{-# LANGUAGE BangPatterns #-}

module Gauss
  ( solveSequential
  , solveParallel
  ) where

import Control.Concurrent (forkOn)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try, throwIO, ErrorCall (..))
import Control.Monad (forM, forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newListArray)
import Matrix

type Augmented = IOUArray Int Double

load :: System -> IO Augmented
load (System n a b) = newListArray (0, n * (n + 1) - 1) (concat (zipWith (\row v -> row ++ [v]) a b))

pivot :: Int -> Augmented -> Int -> IO ()
pivot n m k = do
  let w = n + 1
  best <- findMax (k + 1) k =<< (abs <$> unsafeRead m (k * w + k))
  value <- unsafeRead m (best * w + k)
  when (abs value < 1e-12) $ throwIO (ErrorCall "Матриця вироджена: система не має єдиного розв'язку.")
  when (best /= k) $ forM_ [k .. n] $ \j -> do
    x <- unsafeRead m (k * w + j)
    y <- unsafeRead m (best * w + j)
    unsafeWrite m (k * w + j) y
    unsafeWrite m (best * w + j) x
  where
    findMax !i !bi !bv
      | i >= n = pure bi
      | otherwise = do
          v <- abs <$> unsafeRead m (i * (n + 1) + k)
          if v > bv then findMax (i + 1) i v else findMax (i + 1) bi bv

eliminateRow :: Int -> Augmented -> Int -> Int -> IO ()
eliminateRow n m k i = do
  let w = n + 1
  diag <- unsafeRead m (k * w + k)
  lead <- unsafeRead m (i * w + k)
  let factor = lead / diag
      go !j
        | j > n = pure ()
        | otherwise = do
            pk <- unsafeRead m (k * w + j)
            pi' <- unsafeRead m (i * w + j)
            unsafeWrite m (i * w + j) (pi' - factor * pk)
            go (j + 1)
  unsafeWrite m (i * w + k) 0
  go (k + 1)

eliminateRows :: Int -> Augmented -> Int -> Int -> Int -> IO ()
eliminateRows n m k start step = go start
  where
    go !i
      | i >= n = pure ()
      | otherwise = eliminateRow n m k i >> go (i + step)

backSubstitute :: Int -> Augmented -> IO [Double]
backSubstitute n m = go (n - 1) []
  where
    w = n + 1
    go i acc
      | i < 0 = pure acc
      | otherwise = do
          rhs <- unsafeRead m (i * w + n)
          known <- sum <$> mapM (\(j, x) -> (* x) <$> unsafeRead m (i * w + j)) (zip [i + 1 ..] acc)
          diag <- unsafeRead m (i * w + i)
          go (i - 1) ((rhs - known) / diag : acc)

solveSequential :: System -> IO [Double]
solveSequential s@(System n _ _) = do
  m <- load s
  forM_ [0 .. n - 1] $ \k -> do
    pivot n m k
    eliminateRows n m k (k + 1) 1
  backSubstitute n m

solveParallel :: Int -> System -> IO [Double]
solveParallel threads s@(System n _ _) = do
  m <- load s
  forM_ [0 .. n - 1] $ \k -> do
    pivot n m k
    let workers = min threads (n - k - 1)
    boxes <- forM [0 .. workers - 1] $ \t -> do
      box <- newEmptyMVar
      _ <- forkOn t (try (eliminateRows n m k (k + 1 + t) workers) >>= putMVar box)
      pure box
    forM_ boxes $ \box -> takeMVar box >>= either (\e -> throwIO (e :: SomeException)) pure
  backSubstitute n m
