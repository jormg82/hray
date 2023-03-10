
module Utility.Params(
  Params(..),
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
    pixelSize    = 1,
    sampleNumber = 16
  }

