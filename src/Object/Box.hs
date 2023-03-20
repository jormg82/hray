
module Object.Box(Box(..), box) where


import HRay.ShadeRec
import Object.Object
import qualified Utility.RGBColor as C
import Utility.Constants
import Utility.Ray
import Utility.Utility
import Utility.Vector3D

import Debug.Trace


data Face = XNEG | YNEG | ZNEG | XPOS | YPOS | ZPOS
            deriving (Show, Eq, Ord)

data Box = Box Point3D Point3D C.RGBColor
           deriving Show

box :: Point3D -> Point3D -> C.RGBColor -> Object
box p0 p1 c = Object $ Box p0 p1 c


instance Hittable Box where
  hit (Box p0 p1 col) ray
    | t0 < t1 && t1 > epsilon = Just (tMin, shadeRec)
    | otherwise = Nothing
    where
      (ox, oy, oz)   = (ray&o&x, ray&o&y, ray&o&z)
      (dx, dy, dz)   = (ray&d&x, ray&d&y, ray&d&z)
      (a, b, c)      = (1/dx, 1/dy, 1/dz)

      (txMin, txMax) = if a >= 0
                       then (((p0&x) - ox) * a, ((p1&x) - ox) * a)
                       else (((p1&x) - ox) * a, ((p0&x) - ox) * a)
      (tyMin, tyMax) = if b >= 0
                       then (((p0&y) - oy) * b, ((p1&y) - oy) * b)
                       else (((p1&y) - oy) * b, ((p0&y) - oy) * b)
      (tzMin, tzMax) = if c >= 0
                       then (((p0&z) - oz) * c, ((p1&z) - oz) * c)
                       else (((p1&z) - oz) * c, ((p0&z) - oz) * c)

      (t0, faceIn)   = max3 (txMin, if a>=0 then XNEG else XPOS)
                            (tyMin, if a>=0 then YNEG else YPOS)
                            (tzMin, if a>=0 then ZNEG else ZPOS)
      (t1, faceOut)  = min3 (txMax, if a>=0 then XPOS else XNEG)
                            (tyMax, if a>=0 then YPOS else YNEG)
                            (tzMax, if a>=0 then ZPOS else ZNEG)

      (tMin, norm)   = if t0 > epsilon
                       then (t0, getNormal faceIn)
                       else (t1, getNormal faceOut)
      localHP        = (ray&o) `add` mul (ray&d) tMin
      shadeRec       = ShadeRec{localHitPoint=localHP, normal=norm, color=col}


getNormal :: Face -> Normal
getNormal XNEG = normal3D (-1) 0 0
getNormal YNEG = normal3D 0 (-1) 0
getNormal ZNEG = normal3D 0 0 (-1)
getNormal XPOS = normal3D 1 0 0
getNormal YPOS = normal3D 0 1 0
getNormal ZPOS = normal3D 0 0 1

