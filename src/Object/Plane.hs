
module Object.Plane (Plane(..), plane) where


import HRay.ShadeRec
import Object.Object
import qualified Utility.RGBColor as C
import Utility.Constants
import Utility.Ray
import Utility.Utility
import Utility.Vector3D

data Plane = Plane Point3D Normal C.RGBColor
             deriving Show

plane :: Point3D -> Point3D -> C.RGBColor -> Object
plane p n c = Object $ Plane p n c

instance Hittable Plane where
  hit (Plane point normal color) ray
    | epsilon < t && t < inf = Just (t, sRec)
    | otherwise              = Nothing
    where
      t    = dot (diff point (ray&o)) normal / dot (ray&d) normal
      sRec = ShadeRec (hitPoint ray t) normal color

