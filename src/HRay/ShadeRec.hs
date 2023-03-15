
module HRay.ShadeRec(ShadeRec(..)) where

import Utility.RGBColor
import Utility.Vector3D


data ShadeRec = ShadeRec
  {
    localHitPoint :: Point3D,
    normal        :: Normal,
    color         :: RGBColor -- OJO solo al principio
  }
  deriving Show

