
module Object.Plane (plane) where


import HRay.ShadeRec
import Object.Object
import qualified Utility.RGBColor as C
import Utility.Constants
import Utility.Ray
import Utility.Utility
import Utility.Vector3D


plane :: Point3D -> Normal -> C.RGBColor -> Object
plane p n c = Object $ hitPlane p n c


hitPlane :: Point3D -> Normal -> C.RGBColor -> Hit
hitPlane point normal color ray
  | epsilon < t && t < inf = Just (t, sRec)
  | otherwise              = Nothing
  where
    t    = dot (diff point (ray&o)) normal / dot (ray&d) normal
    sRec = ShadeRec (hitPoint ray t) normal color

