
module Utility.Point2D (
  Point2D(x, y),
  point2D,
  mul
) where



data Point2D = Point2D {x :: !Double, y :: !Double}
               deriving Show


point2D :: Double -> Double -> Point2D
point2D = Point2D


mul :: Point2D -> Double -> Point2D
mul (Point2D a b) x = Point2D (a*x) (b*x)
