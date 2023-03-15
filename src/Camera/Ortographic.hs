
module Camera.Ortographic(ortographic) where

import Camera.Camera
import HRay.State
import Tracer.Tracer
import qualified Utility.RGBColor as C
import qualified Utility.Point2D as P
import Utility.Ray
import Utility.Utility
import Utility.Vector3D

import Control.Monad(forM, when)


ortographic :: Camera
ortographic tracer = do
  vres <- getVRes
  hres <- getHRes
  let rows    = [vres-1, vres-2..0]
      columns = [0..hres-1]
      pairs   = [(r, c) | r <- rows, c <- columns]
  forM pairs (processPixel tracer)


processPixel :: Tracer -> (Int, Int) -> HR C.RGBColor
processPixel tracer (r, c) = do
  when (c == 0) $ writeErrLn $ "Scanlines remaining: " ++ show r
  samples <- sampleUnitSquare
  (hres, vres, s) <- (,,) <$> getHRes <*> getVRes <*> getPixelSize
  let x      = s * (fromIntegral c - 0.5*fromIntegral hres)
      y      = s * (fromIntegral r - 0.5*fromIntegral vres)
  colors <- traverse (tracer . genRay x y s) samples
  nsamples <- getNumSamples
  -- Se devuelve una computacion pura, forzamos su evaluacion
  return $! foldr1 C.add colors `C.divi` (fromIntegral nsamples)


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

