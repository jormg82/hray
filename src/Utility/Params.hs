
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
    imageWidth   = 512,
    aspectRatio  = 1%1,
    pixelSize    = 10.83 / 512,
    sampleNumber = 256
  }

