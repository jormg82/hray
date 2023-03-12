{-# LANGUAGE RecordWildCards #-}

module Sampler.Sampler (
  Sample,
  Sampler(..),
  SampleGenFn,
  generateSampler,
  sampleUnitSquare,
  sampleUnitDisk,
  genRandomSample,
  genRegularSample
) where

import Utility.Point2D
import Utility.Utility

import Data.Array
import System.Random


type Sample       = Point2D
type SampledShape = [Sample]

                       -- Num samples, num sets
data Sampler = Sampler
  {
    numSamples     :: Int,
    numSets        :: Int,
    unitSquareSets :: Array Int SampledShape,
    unitDiskSets   :: Array Int SampledShape
  }
  deriving Show


type SampleGenFn = Int -> IO SampledShape

generateSampler :: Int -- Number of samples
                -> Int -- Number of sets
                -> SampleGenFn
                -> IO Sampler
generateSampler samples sets gen = do
  let rSamples = replicate sets samples
  xs <- mapM gen rSamples
  ys <- mapM ((map sampleToUnitDisk <$>) . gen) rSamples
  let array1 = listArray (0, sets-1) xs
      array2 = listArray (0, sets-1) ys
  return $ Sampler samples sets array1 array2


sampleUnitSquare :: Sampler -> IO SampledShape
sampleUnitSquare Sampler{..} = (unitSquareSets !) <$> randomRIO (0, numSets-1)


sampleUnitDisk :: Sampler -> IO SampledShape
sampleUnitDisk Sampler{..} = (unitDiskSets !) <$> randomRIO (0, numSets-1)


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


sampleToUnitDisk :: Sample -> Sample
sampleToUnitDisk p
  | a == 0 || b == 0 = p 
  | a > (-b)  = if a > b
                then point2D (a2/den) (a*b/den)
                else point2D (a*b/den) (b2/den)
  | otherwise = if a < b
                then point2D ((-a2)/den) ((-a)*b/den)
                else point2D ((-a)*b/den) ((-b2)/den)
  where
    (a, b, a2, b2) = (2*(p&x)-1, 2*(p&y)-1, a*a, b*b)
    den = sqrt $ a2 + b2

