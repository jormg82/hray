
module Tracer.Simple(simple) where

import HRay.ShadeRec
import HRay.HRay
import Object.Object


simple :: Tracer
simple ray = do
  sc <- getScene
  case hit (objects sc) ray of
    Nothing        -> return $ backgroundColor sc
    Just (_, sRec) -> return $ color sRec

