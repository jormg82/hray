
module Camera.Camera (
  RenderFn,
  Camera(renderScene),
  camera
) where

import Sampler.Sampler
import Tracer.Tracer
import Utility.RGBColor
import Utility.Scene
import World.ViewPlane


type RenderFn = Scene -> ViewPlane -> Sampler -> Tracer -> IO [RGBColor]

data Camera = Camera
  {
    renderScene :: RenderFn
  }

camera :: RenderFn -> Camera
camera = Camera

