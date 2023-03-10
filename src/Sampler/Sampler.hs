
module Sampler.Sampler (
  Sample, SampledUnitSquare,
  Sampler(numSamples, numSets, sets),
  SampleGenFn,
  generateSampler,
  sampleUnitSquare,
  genRandomSample,
  genRegularSample
) where

import Utility.Point2D
import Utility.Utility

import Data.Array
import System.Random


type Sample = Point2D
type SampledUnitSquare = [Sample]

                       -- Num samples, num sets
data Sampler = Sampler
  {
    numSamples :: Int,
    numSets    :: Int,
    sets       :: Array Int SampledUnitSquare
  }
  deriving Show

type SampleGenFn = Int -> IO SampledUnitSquare


generateSampler :: Int -- Number of samples
                -> Int -- Number of sets
                -> SampleGenFn
                -> IO Sampler
generateSampler samples sets gen = do
  xs <- mapM gen (replicate sets samples)
  return $ Sampler samples sets $ listArray (0, sets-1) xs


sampleUnitSquare :: Sampler -> IO SampledUnitSquare
sampleUnitSquare (Sampler _ n arr) = (arr !) <$> randomRIO (0, n-1)


genRandomSample :: Int  -- Number of divisions of x and y
          -> (Int, Int) -- Integer coordinates 
          -> IO Sample
genRandomSample n (p, q) = do
  let n' = fromIntegral n
      p' = fromIntegral p
      q' = fromIntegral q
  x <- randomDouble
  y <- randomDouble
  return $ point2D ((q'+x)/n') ((p'+y)/n')


-- Like genRandomSample, but not random
genRegularSample :: Int
                 -> (Int, Int)
                 -> Sample
genRegularSample n (p, q) = point2D ((q'+0.5)/n') ((p'+0.5)/n')
  where
    n' = fromIntegral n
    p' = fromIntegral p
    q' = fromIntegral q

