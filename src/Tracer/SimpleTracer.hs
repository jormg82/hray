
module Tracer.SimpleTracer(simpleTracer) where

import HRay.Scene
import HRay.ShadeRec
import HRay.State
import Object.Object
import Tracer.Tracer


simpleTracer :: Tracer
simpleTracer ray = do
  sc <- getScene
  case hit (objects sc) ray of
    Nothing        -> return $ backgroundColor sc
    Just (_, sRec) -> return $ color sRec

