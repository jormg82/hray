
module Sampler.MultiJittered(multiJittered) where

import Sampler.Sampler
import Utility.Point2D
import Utility.Utility

import Data.List(transpose)
import Data.List.Split(chunksOf)

multiJittered :: SampleGen
multiJittered n = do
  let sq = floor $ sqrt $ fromIntegral n
      ns = [0..n-1]
  ns' <- (concat . transpose) <$> traverse randomShuffle (chunksOf sq ns)
  traverse (genRandomSample n) (zip ns ns')

