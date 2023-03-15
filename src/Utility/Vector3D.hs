{-# LANGUAGE RecordWildCards #-}

module Utility.Vector3D (
  Vector3D(..),
  Normal, Point3D, ONB(..),
  vector3D, point3D, normal3D,
  add, diff, mul, divi,
  dot, cross,
  normalize
) where



data Vector3D = Vector3D {x :: !Double, y :: !Double, z :: !Double}
                deriving Show

type Normal = Vector3D
type Point3D = Vector3D

data ONB = ONB {u :: Vector3D, v :: Vector3D, w :: Vector3D}
           deriving Show

vector3D :: Double -> Double -> Double -> Vector3D
vector3D = Vector3D

point3D :: Double -> Double -> Double -> Vector3D
point3D = Vector3D

normal3D :: Double -> Double -> Double -> Normal
normal3D = Vector3D

add :: Vector3D -> Vector3D -> Vector3D
add (Vector3D a b c) (Vector3D a' b' c') = Vector3D (a+a') (b+b') (c+c')

diff :: Vector3D -> Vector3D -> Vector3D
diff (Vector3D a b c) (Vector3D a' b' c') = Vector3D (a-a') (b-b') (c-c')

mul :: Vector3D -> Double -> Vector3D
mul (Vector3D a b c) x = Vector3D (a*x) (b*x) (c*x)

divi :: Vector3D -> Double -> Vector3D
divi (Vector3D a b c) x = Vector3D (a/x) (b/x) (c/x)

dot :: Vector3D -> Vector3D -> Double
dot (Vector3D a b c) (Vector3D a' b' c') = a*a' + b*b' + c*c'

cross :: Vector3D -> Vector3D -> Vector3D
cross (Vector3D a b c) (Vector3D a' b' c') =
  Vector3D (b*c'-b'*c) (a'*c-a*c') (a*b'-a'*b)

normalize :: Vector3D -> Vector3D
normalize v@Vector3D{..} = v `divi` (sqrt $ x*x + y*y + z*z)

