
module Sampler.Jittered(jittered) where

import Sampler.Sampler
import Utility.Point2D
import Utility.Utility


jittered :: SampleGen
jittered n = do
  let sq = floor $ sqrt $ fromIntegral n
      ns = [0..sq-1]
      ps = [(p, q) | p <- ns, q <- ns]
  traverse (genRandomSample sq) ps

