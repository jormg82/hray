
module Utility.Ray (
  Ray(o, d),
  ray,
  hitPoint
) where

import Utility.Utility
import Utility.Vector3D


data Ray = Ray {o :: Point3D, d :: Vector3D}
               deriving Show

ray :: Point3D -> Vector3D -> Ray
ray = Ray


hitPoint :: Ray -> Double -> Point3D
hitPoint r t = add (r&o) (mul (r&d) t)
