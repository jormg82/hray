
module Object.Triangle(Triangle(), triangle) where


import HRay.ShadeRec
import Object.Object
import qualified Utility.RGBColor as C
import Utility.Constants
import qualified Utility.Ray as R
import Utility.Utility
import Utility.Vector3D

import Debug.Trace


data Triangle = Triangle
  {
    v0     :: Point3D,
    v1     :: Point3D,
    v2     :: Point3D,
    normal :: Normal,
    color  :: C.RGBColor
  }
  deriving Show


triangle :: Point3D -> Point3D -> Point3D -> C.RGBColor -> Object
triangle v0 v1 v2 c = Object $ Triangle v0 v1 v2 normal c
  where normal = diff v1 v0 `cross` diff v2 v0


instance Hittable Triangle where
  hit (Triangle v0 v1 v2 normal col) ray
    | beta < 0         = Nothing
    | gamma < 0        = Nothing
    | (beta + gamma) > 1 = Nothing
    | t < epsilon      = Nothing
    | otherwise        = Just (t, shadeRec)
    where
      a = (v0&x)-(v1&x)
      b = (v0&x)-(v2&x)
      c = ray&R.d&x
      d = (v0&x)-(ray&R.o&x)
      e = (v0&y)-(v1&y)
      f = (v0&y)-(v2&y)
      g = ray&R.d&y
      h = (v0&y)-(ray&R.o&y)
      i = (v0&z)-(v1&z)
      j = (v0&z)-(v2&z)
      k = ray&R.d&z
      l = (v0&z)-(ray&R.o&z)
      m = f*k - g*j
      n = h*k - g*l
      p = f*l - h*j
      q = g*i - e*k
      s = e*j - f*i
      invDenom = 1 / (a*m + b*q + c*s)
      e1 = d*m - b*n - c*p
      beta = e1 * invDenom
      r = e*l - h*i
      e2 = a*n + d*q + c*r
      gamma = e2 * invDenom
      e3 = a*p - b*r - d*s
      t = e3 * invDenom
      localHP = (ray&R.o) `add` mul (ray&R.d) t
      shadeRec = ShadeRec localHP normal col

