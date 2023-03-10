
module Utility.Scene (
  Scene(objects, backgroundColor),
  scene
) where

import Object.Object
import Utility.RGBColor


data Scene = Scene {objects :: [Object], backgroundColor :: RGBColor}


scene :: [Object] -> RGBColor -> Scene
scene = Scene
