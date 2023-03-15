{-# LANGUAGE RecordWildCards #-}

module Sampler.Sampler (
  Sample2D, Sample3D,
  Sampler(..),
  SampleGen,
  generateSampler,
  unitSquare,
  unitDisk,
  hemisphere,
  genRandomSample,
  genRegularSample,
  mapSamplesToHemisphere
) where


import Utility.Constants
import Utility.Point2D
import Utility.Utility
import Utility.Vector3D(point3D, Point3D)

import Data.Array
import System.Random


type Sample2D = Point2D
type Sample3D = Point3D

 
data Sampler = Sampler
  {
    numSamples     :: Int,
    numSets        :: Int,
    unitSquareSets :: Array Int [Sample2D],
    unitDiskSets   :: Array Int [Sample2D],
    hemisphereSets :: Array Int [Sample3D]
  }
  deriving Show


type SampleGen = Int -> IO [Sample2D]


generateSampler :: Int -- Number of samples
                -> Int -- Number of sets
                -> SampleGen
                -> IO Sampler
generateSampler samples sets gen = do
  let rSamples = replicate sets samples
  as <- mapM gen rSamples
  bs <- mapM ((map sampleToUnitDisk <$>) . gen) rSamples
  let array1 = listArray (0, sets-1) as
      array2 = listArray (0, sets-1) bs
      -- OJO inicializo con e=0
      as'    = map (map $ sampleToHemisphere 0) as
      array3 = listArray (0, sets-1) as'
  return $ Sampler samples sets array1 array2 array3


genRandomSample :: Int  -- Number of divisions of x and y
                -> (Int, Int) -- Integer coordinates 
                -> IO Sample2D
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
                 -> Sample2D
genRegularSample n (p, q) = point2D ((q'+0.5)/n') ((p'+0.5)/n')
  where
    n' = fromIntegral n
    p' = fromIntegral p
    q' = fromIntegral q


unitSquare :: Sampler -> IO [Sample2D]
unitSquare Sampler{..} = (unitSquareSets !) <$> randomRIO (0, numSets-1)

unitDisk :: Sampler -> IO [Sample2D]
unitDisk Sampler{..} = (unitDiskSets !) <$> randomRIO (0, numSets-1)

hemisphere :: Sampler -> IO [Sample3D]
hemisphere Sampler{..} = (hemisphereSets !) <$> randomRIO (0, numSets-1)


sampleToUnitDisk :: Sample2D -> Sample2D
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


sampleToHemisphere :: Double -> Sample2D -> Sample3D
sampleToHemisphere e p =
  point3D (sinTheta*cosPhi) (sinTheta*sinPhi) cosTheta
  where
    cosPhi   = cos $ twopi * (p&x)
    sinPhi   = sin $ twopi * (p&x)
    cosTheta = (1 - (p&y)) ** (1 / (e+1))
    sinTheta = sqrt $ 1 - cosTheta*cosTheta


mapSamplesToHemisphere :: Double -> Sampler -> Sampler
mapSamplesToHemisphere e s@Sampler{..} =
  s{hemisphereSets=listArray (0, numSets-1) hsets}
  where
    hsets = map (map $ sampleToHemisphere e) (elems unitSquareSets)

