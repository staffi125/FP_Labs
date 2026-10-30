{-# LANGUAGE BangPatterns #-}

module Matrix
  ( System (..)
  , randomSystem
  , readSystem
  , multiply
  , maxError
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.Word (Word64)

data System = System
  { size        :: !Int
  , coefficients :: [[Double]]
  , rightSide   :: [Double]
  }

lcg :: Word64 -> Word64
lcg s = s * 6364136223846793005 + 1442695040888963407

uniform :: Word64 -> (Double, Word64)
uniform s =
  let s' = lcg s
      bits = (s' `xor` (s' `shiftR` 29)) .&. 0xFFFFFFFFFFFF
   in (fromIntegral bits / 281474976710656 * 2 - 1, s')

randomRows :: Int -> Word64 -> [[Double]]
randomRows n seed = go n seed
  where
    go 0 _ = []
    go k s =
      let (row, s') = takeRow n s
       in row : go (k - 1) s'
    takeRow 0 s = ([], s)
    takeRow m s =
      let (v, s1) = uniform s
          (rest, s2) = takeRow (m - 1) s1
       in (v : rest, s2)

randomSystem :: Int -> Word64 -> (System, [Double])
randomSystem n seed =
  let rows = randomRows n seed
      dominant = [ [ if i == j then x + fromIntegral n else x | (j, x) <- zip [0 :: Int ..] row ]
                 | (i, row) <- zip [0 :: Int ..] rows ]
      solution = [fromIntegral (i `mod` 7) - 3 | i <- [1 .. n]]
   in (System n dominant (multiply dominant solution), solution)

multiply :: [[Double]] -> [Double] -> [Double]
multiply a x = [sum (zipWith (*) row x) | row <- a]

maxError :: [Double] -> [Double] -> Double
maxError xs ys = maximum (0 : zipWith (\a b -> abs (a - b)) xs ys)

readSystem :: String -> Either String System
readSystem text = case map (map read . words) (filter (not . null . words) (lines text)) of
  ([n'] : rows)
    | n <- round (n' :: Double)
    , length rows == n
    , all ((== n + 1) . length) rows ->
        Right (System n (map (take n) rows) (map last rows))
  _ -> Left "Очікується: у першому рядку n, далі n рядків по n + 1 числу (коефіцієнти і вільний член)."
