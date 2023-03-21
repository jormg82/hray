
module Main where

import Camera.Function
--import Camera.Pinhole
import Camera.ThinLens
import HRay.HRay
import Object.Box
import Object.Object
import Object.Plane
import Object.Sphere
import Object.Triangle
import Sampler.Sampler
import Sampler.MultiJittered
import Tracer.Simple
import qualified Utility.RGBColor as C
import qualified Utility.Point2D as P
import Utility.Utility
import Utility.Vector3D

import qualified Debug.Trace as D
import Data.Foldable(traverse_)
import Data.Ratio


renderWorld :: HR ()
renderWorld = do
  (hres, vres) <- (,) <$> getHRes <*> getVRes

  -- ppm file header
  writeStdLn "P3"
  writeStdLn $ show hres ++ " " ++ show vres
  writeStdLn "255"

  cs <- getCamera >>= render

  -- ppm file data
  traverse_ (writeStdLn . C.showColor) cs

  writeErrLn ""


scene1 :: Scene
scene1  = Scene [sphere (point3D 0 0 0) 70 C.red] C.black

scene2 :: Scene
scene2 = Scene [sphere (point3D 0 (-25) 0) 80 C.red,
                sphere (point3D 0 30 0) 60 (C.rgbcolor 1 1 0),
                --plane (point3D 0 0 0) (normal3D 0 1 1) (C.rgbcolor 0 0.3 0)]
                plane (point3D 0 0 0) (normal3D 0 1 0) (C.rgbcolor 0 0.3 0)]
               C.black

scene3 :: Scene
scene3  = Scene [box (point3D (-100) 0 (-200))
                     (point3D (-50) 200 (-150)) C.blue,
                 box (point3D (-200) 0 (-100))
                     (point3D (-150) 100 (-50)) C.blue,
                 plane (point3D 0 (-10) 0) (normal3D 0 1 0) (C.gray 0.3),
                 box (point3D (-100) 0 (-800))
                     (point3D (-50) 300 (-550)) C.blue] C.black

scene4 :: Scene
scene4  = Scene [triangle (point3D (-30) 59 10)
                          (point3D 99 (-99) 0)
                          (point3D 40 39 0) C.blue] C.black


build :: IO HRState
build = do
  let cam = thinLens $ ThinLens {
              eye    = point3D 0 10 150,
              lookat = point3D 0 0 0,
              up     = vector3D 0 1 0,
              d      = 200,
              lensRadius = 10,
              f      = 300,
              zoom   = 1.0,
              exposureTime = 1.0
            }
      sc  = scene3
      tr = simple
      vp  = ViewPlane {
              hres  = 600,
              vres  = 400,
              s     = 1.0,
              gamma = 1.0
            }
      sampleNumber = 16
      nSet = 83
  sp <- generateSampler sampleNumber nSet multiJittered

  return $ HRState{camera=cam, sampler=sp, scene=sc, tracer=tr, viewPlane=vp}


main :: IO ()
main = do
  state <- build
  res <- runHR renderWorld state
  case res of
    Left s   -> putStrLn s
    Right () -> return ()

