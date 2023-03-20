
module Object.Sphere(Sphere(..), sphere) where

import HRay.ShadeRec
import Object.Object
import qualified Utility.RGBColor as C
import Utility.Constants
import Utility.Ray
import Utility.Utility
import Utility.Vector3D


data Sphere = Sphere Point3D Double C.RGBColor 
              deriving Show

sphere :: Point3D -> Double -> C.RGBColor -> Object
sphere p r c = Object $ Sphere p r c


instance Hittable Sphere where
  hit (Sphere center radius color) ray
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
      sRec x = ShadeRec (hitPoint ray x) norm color

