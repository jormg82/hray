
module Utility.Point2D (
  Point2D(x, y),
  point2D
) where



data Point2D = Point2D {x :: !Double, y :: !Double}
               deriving Show


point2D :: Double -> Double -> Point2D
point2D = Point2D

