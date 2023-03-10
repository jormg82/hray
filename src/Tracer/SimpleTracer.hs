
module Tracer.SimpleTracer(simpleTracer) where

import Object.Object
import Utility.Scene
import Utility.ShadeRec
import Tracer.Tracer


simpleTracer :: Scene -> Tracer
simpleTracer = tracer . trace

trace :: Scene -> TracerFn
trace scene ray =
  case hit ray (objects scene) of
    Nothing        -> backgroundColor scene
    Just (_, sRec) -> color sRec

