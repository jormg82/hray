
module Sampler.Regular(regular) where

import Sampler.Sampler
import Utility.Point2D

import Control.Monad(forM)


regular :: SampleGen
regular n = do
  let sq = floor $ sqrt $ fromIntegral n
      ns = [0..sq-1]
      ps = [(p, q) | p <- ns, q <- ns]
  return $ map (genRegularSample sq) ps

