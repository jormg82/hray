
module Object.Sphere (sphere) where

import Object.Object
import qualified Utility.RGBColor as C
import Utility.Constants
import Utility.Ray
import Utility.ShadeRec
import Utility.Utility
import Utility.Vector3D


sphere :: Point3D -> Double -> C.RGBColor -> Object
sphere p r c = object $ hitSphere p r c


hitSphere :: Point3D -> Double -> C.RGBColor -> HitObjectFn
hitSphere center radius color ray
  | disc < 0     = Nothing
  | t > epsilon  = Just (t, sRec t)
  | t' > epsilon = Just (t', sRec t')
  | otherwise    = Nothing
  where
    temp   = (ray&o) `diff` center
    a      = (ray&d) `dot` (ray&d)
    b      = 2.0*(temp `dot` (ray&d))
    c      = temp `dot` temp - radius*radius
    disc   = b*b - 4.0*a*c
    e      = sqrt disc
    denom  = 2.0*a
    norm   = (temp `add` (ray&d)) `divi` radius
    t      = ((-b) - e) / denom
    t'     = ((-b) + e) / denom
    sRec x = shadeRec (hitPoint ray x) norm color

