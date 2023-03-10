
module Tracer.TestSampler(testSampler) where

import Tracer.Tracer
import Utility.Ray
import Utility.RGBColor
import Utility.Utility
import Utility.Vector3D


testSampler :: Tracer
testSampler = tracer trace


-- Render in a 512x512 window, and (x, y) in [0, 10.83]^2
trace :: TracerFn
trace r = rgbcolor c c c
  where
    delta = 10.83 / 2.0
    x'    = (r&o&x) + delta
    y'    = (r&o&y) + delta 
    c     = 0.5 * (1 + sin (x'*x'*y'*y'))
