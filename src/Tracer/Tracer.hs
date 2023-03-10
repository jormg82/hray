module Tracer.Tracer (
  TraceRayFn,
  Tracer(traceRay),
  tracer
) where

import Utility.RGBColor
import Utility.Ray
import Utility.Scene

type TraceRayFn = Scene -> Ray -> RGBColor

data Tracer = Tracer {traceRay :: TraceRayFn}

tracer :: TraceRayFn -> Tracer
tracer = Tracer

