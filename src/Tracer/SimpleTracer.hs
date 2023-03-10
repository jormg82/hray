
module Tracer.SimpleTracer (simpleTracer) where

import Object.Object
import Utility.Scene
import Utility.ShadeRec
import Tracer.Tracer


simpleTracer :: Tracer
simpleTracer = tracer trace

trace :: TraceRayFn
trace scene ray =
  case hit ray (objects scene) of
    Nothing        -> backgroundColor scene
    Just (_, sRec) -> color sRec

