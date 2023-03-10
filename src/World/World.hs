
module World.World (
  World(),
  build,
  renderWorld
) where


import Camera.Camera
import Camera.Ortographic
import Object.Plane
import Object.Sphere
import Sampler.Jittered
import Sampler.MultiJittered
import Sampler.PureRandom
import Sampler.Regular
import Sampler.Sampler
import Tracer.SimpleTracer
import Tracer.TestSampler
import Tracer.Tracer
import Utility.Params
import qualified Utility.RGBColor as C
import Utility.Scene
import Utility.Utility
import Utility.Vector3D
import World.ViewPlane

import Data.Ratio


data World = World Scene ViewPlane Sampler Tracer Camera


scene1 :: Scene
--OJO reponer
--scene1  = scene [sphere (point3D 0 0 0) 85 C.red] C.black
scene1  = scene [sphere (point3D 0 0 0) 85 C.red] C.black

scene2 :: Scene
scene2 = scene [sphere (point3D 0 (-25) 0) 80 C.red,
                sphere (point3D 0 30 0) 60 (C.rgbcolor 1 1 0),
                plane (point3D 0 0 0) (normal3D 0 1 1) (C.rgbcolor 0 0.3 0)]
               C.black


-- Aqui se podria poner como argumento un WorldParam
build :: Params -> IO World
build par = do
  let sc   = scene2
      vp   = ViewPlane {
               hres  = imageWidth par,
               vres  = floor $ (fromIntegral $ imageWidth par) /
                               aspectRatio par,
               s     = pixelSize par,
               gamma = 1.0
             }
      tr   = simpleTracer sc
      cam  = ortographic
      nSet = 83
  sp <- generateSampler (sampleNumber par) nSet multiJittered
  return $ World sc vp sp tr cam
 

renderWorld :: World -> IO ()
renderWorld (World sc vp sp tr cam) = do

  -- ppm file header
  putStrLn "P3"
  putStrLn $ show (vp&hres) ++ " " ++ show (vp&vres)
  putStrLn "255"

  cs <- renderScene cam sc vp sp tr

  -- ppm file data
  mapM_ (putStrLn . C.showColor) cs

