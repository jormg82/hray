
module Tracer.TestSampler(testSampler) where

import HRay.State
import Tracer.Tracer
import Utility.Ray
import Utility.RGBColor
import Utility.Utility
import Utility.Vector3D


-- Render in a 512x512 window, and (x, y) in [0, 10.83]^2
testSampler :: Double -> Tracer
testSampler delta r = return $ gray c
  where
    --delta = 10.83 / 2.0
    x'    = (r&o&x) + delta
    y'    = (r&o&y) + delta 
    c     = 0.5 * (1 + sin (x'*x'*y'*y'))
