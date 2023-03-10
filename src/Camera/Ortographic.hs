
module Camera.Ortographic (ortographic) where

import Camera.Camera
import Sampler.Sampler
import Tracer.Tracer
import qualified Utility.RGBColor as C
import qualified Utility.Point2D as P
import Utility.Scene
import Utility.Ray
import Utility.Utility
import Utility.Vector3D
import World.ViewPlane

import Control.Monad(forM, when)


ortographic :: Camera
ortographic = camera render


render :: RenderFn
render sc vp sp tr = do
  let rows    = [(vp&vres)-1, (vp&vres)-2..0]
      columns = [0..(vp&hres)-1]
      pairs   = [(r, c) | r <- rows, c <- columns]
  forM pairs (processPixel sc vp sp tr)


processPixel :: Scene
             -> ViewPlane
             -> Sampler
             -> Tracer
             -> (Int, Int) -- (row, column)
             -> IO C.RGBColor
processPixel sc vp sp tr (r, c) = do
  when (c == 0) $ messageStdErr $ "Scanlines remaining: " ++ show r
  samples <- sampleUnitSquare sp
  let x      = (vp&s) * (fromIntegral c - 0.5*fromIntegral (vp&hres))
      y      = (vp&s) * (fromIntegral r - 0.5*fromIntegral (vp&vres))
      colors = map (traceRay tr sc . genRay x y (vp&s)) samples
  C.seqColor $ foldr1 C.add colors `C.divi` (fromIntegral $ numSamples sp)


genRay :: Double    -- x
       -> Double    -- y
       -> Double    -- s
       -> P.Point2D -- Point in [0,1]^2
       -> Ray
genRay x y s p = ray (point3D (p'&P.x) (p'&P.y) zw) dir
  where
    zw = 100
    dir = vector3D 0 0 (-1)
    p' = P.point2D (x + s*(p&P.x)) (y + s*(p&P.y))

