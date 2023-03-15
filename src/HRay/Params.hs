
module HRay.Params(
  Params(
    imageWidth,
    aspectRatio,
    pixelSize,
    sampleNumber
  ),
  defaultParams
) where

import Data.Ratio


data Params = Params
  {
    imageWidth   :: Int,
    aspectRatio  :: Rational,
    pixelSize    :: Double,
    sampleNumber :: Int
  }

defaultParams = Params
  {
    imageWidth   = 300,
    aspectRatio  = 3%2,
    pixelSize    = 1.0,
    sampleNumber = 16
  }

