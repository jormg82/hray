
module Utility.Utility (
  degreesToRadians,
  clamp, (&),
  randomDouble,
  randomShuffle,
  max3, min3
) where

import Utility.Constants
import Data.Function((&))

import System.Random


degreesToRadians :: Double -> Double
degreesToRadians = (*pion180)


clamp :: Double -- Value
      -> Double -- Min
      -> Double -- Max
      -> Double
clamp val m n | val < m   = m
              | val > n   = n
              | otherwise = val


-- Devuelve un valor en [0, 1]
randomDouble :: IO Double
randomDouble = do
  a <- randomRIO (0, maxBound) :: IO Int
  return $ fromIntegral a / fromIntegral (maxBound :: Int)


randomShuffle :: [a] -> IO [a]
randomShuffle [] = return []
randomShuffle xs@(_:_) = do
  let maxElem = length xs - 1
  rpos <- randomRIO (0, maxElem)     
  let (xs', y:ys') = splitAt rpos xs
  (y:) <$> randomShuffle (xs'++ys')


max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)


min3 :: Ord a => a -> a -> a -> a
min3 a b c = min a (min b c)
