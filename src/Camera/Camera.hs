
module Camera.Camera(Camera) where

import HRay.State
import Tracer.Tracer
import Utility.RGBColor

type Camera = Tracer -> HR [RGBColor]
