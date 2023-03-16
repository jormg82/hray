
module Main where

import Camera.Camera
import Camera.Pinhole
import HRay.Scene
import HRay.State
import HRay.ViewPlane
import Object.Object
import Object.Plane
import Object.Sphere
import Sampler.Sampler
import Sampler.MultiJittered
import Tracer.Tracer
import Tracer.SimpleTracer
import qualified Utility.RGBColor as C
import Utility.Utility
import Utility.Vector3D

import Data.Foldable(traverse_)
import Data.Ratio


renderWorld :: HR [C.RGBColor] -> HR ()
renderWorld render = do
  (hres, vres) <- (,) <$> getHRes <*> getVRes

  -- ppm file header
  writeStdLn "P3"
  writeStdLn $ show hres ++ " " ++ show vres
  writeStdLn "255"

  cs <- render

  -- ppm file data
  traverse_ (writeStdLn . C.showColor) cs

  writeErrLn ""


scene1 :: Scene
scene1  = Scene [sphere (point3D 0 0 0) 85 C.red] C.black

scene2 :: Scene
scene2 = Scene [sphere (point3D 0 (-25) 0) 80 C.red,
                sphere (point3D 0 30 0) 60 (C.rgbcolor 1 1 0),
                --plane (point3D 0 0 0) (normal3D 0 1 1) (C.rgbcolor 0 0.3 0)]
                plane (point3D 0 0 0) (normal3D 0 1 0) (C.rgbcolor 0 0.3 0)]
               C.black

pinholecnf :: PinholeCnf
pinholecnf = PinholeCnf
  {
    eye    = point3D 0 0 100,
    lookat = point3D 0 0 0,
    up     = vector3D 0 1 0,
    d      = 50.0,
    zoom   = 1.0,
    exposureTime = 1.0
  }


build :: IO HRState
build = do
  let sc   = scene2
      vp   = ViewPlane {
               hres  = 300,
               vres  = 200,
               s     = 1.0,
               gamma = 1.0
             }
      sampleNumber = 16
      nSet = 83
  sp <- generateSampler sampleNumber nSet multiJittered
  return $ HRState{sampler=sp, scene=sc, viewPlane=vp}


main :: IO ()
main = do
  state <- build

  let renderW = renderWorld $ (pinhole pinholecnf) simpleTracer

  res <- runHR renderW state
  case res of
    Left s   -> putStrLn s
    Right () -> return ()

