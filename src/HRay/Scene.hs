
module HRay.Scene(Scene(..)) where

import Object.Object
import Utility.RGBColor


data Scene = Scene {objects :: [Object], backgroundColor :: RGBColor}

