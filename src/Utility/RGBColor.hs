{-# LANGUAGE RecordWildCards #-}

module Utility.RGBColor (
  --RGBColor(r, g, b),
  RGBColor(..),
  rgbcolor, gray,
  add, mul, divi,
  seqColor,
  showColor,
  red, green, blue,
  black, white
) where

import Utility.Utility


data RGBColor = RGBColor {r :: !Double, g :: !Double, b :: !Double}
                deriving Show

rgbcolor :: Double -> Double -> Double -> RGBColor
rgbcolor = RGBColor

gray :: Double -> RGBColor
gray c = RGBColor c c c

add :: RGBColor -> RGBColor -> RGBColor
add (RGBColor r g b) (RGBColor r' g' b') = RGBColor (r+r') (g+g') (b+b')

mul :: RGBColor -> Double -> RGBColor
mul RGBColor{..} d = RGBColor (r*d) (g*d) (b*d)

divi :: RGBColor -> Double -> RGBColor
divi RGBColor{..} d = RGBColor (r/d) (g/d) (b/d)

seqColor :: RGBColor -> IO RGBColor
seqColor c@(RGBColor r' g' b') =
  r' `seq` g' `seq` b' `seq` (return $ c{r=r', g=g', b=b'})

showColor :: RGBColor -> String
showColor RGBColor{..} = show r' ++ " " ++ show g' ++ " " ++ show b'
  where
    r' = floor (256 * clamp r 0 0.999) :: Int
    g' = floor (256 * clamp g 0 0.999) :: Int
    b' = floor (256 * clamp b 0 0.999) :: Int


red, green, blue :: RGBColor
red   = RGBColor {r=1, g=0, b=0}
green = RGBColor {r=0, g=1, b=0}
blue  = RGBColor {r=0, g=0, b=1}


black, white :: RGBColor
black = RGBColor {r=0, g=0, b=0}
white = RGBColor {r=1, g=1, b=1}

