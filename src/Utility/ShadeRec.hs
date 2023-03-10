
module Utility.ShadeRec (
  ShadeRec(localHitPoint, normal, color),
  shadeRec
) where

import Utility.RGBColor
import Utility.Vector3D


data ShadeRec = ShadeRec
  {
    localHitPoint :: Point3D,
    normal        :: Normal,
    color         :: RGBColor -- OJO solo al principio
  }
  deriving Show


shadeRec :: Point3D -> Normal -> RGBColor -> ShadeRec
shadeRec = ShadeRec

