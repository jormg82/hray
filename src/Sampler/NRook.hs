
module Sampler.NRook(nrook) where

import Sampler.Sampler
import Utility.Point2D
import Utility.Utility


nrook :: SampleGen
nrook n = do
  let ns  = [0..n-1]
  ns' <- randomShuffle ns
  traverse (genRandomSample n) (zip ns ns')

