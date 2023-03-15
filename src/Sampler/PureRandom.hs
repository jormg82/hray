
module Sampler.PureRandom (pureRandom) where

import Sampler.Sampler
import Utility.Point2D
import Utility.Utility

import Control.Monad(replicateM)

pureRandom :: SampleGen
pureRandom n = replicateM n $ point2D <$> randomDouble <*> randomDouble

