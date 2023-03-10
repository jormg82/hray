module Tracer.Tracer (
  TracerFn,
  Tracer(traceRay),
  tracer
) where

import Utility.RGBColor
import Utility.Ray


type TracerFn = Ray -> RGBColor

data Tracer = Tracer {traceRay :: TracerFn}

tracer :: TracerFn -> Tracer
tracer = Tracer

