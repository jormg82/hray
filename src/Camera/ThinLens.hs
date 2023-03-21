{-# LANGUAGE RecordWildCards #-}

module Camera.ThinLens(ThinLens(..), thinLens) where

import HRay.HRay
import Sampler.Sampler
import qualified Utility.RGBColor as C
import Utility.Point2D(point2D, Point2D)
import qualified Utility.Point2D as P
import Utility.Ray
import Utility.Utility
import Utility.Vector3D

import Control.Monad(when)


data ThinLens = ThinLens
  {
    eye          :: Eye,
    lookat       :: LookAt,
    up           :: Up,
    d            :: Double, -- ViewPlane distance
    lensRadius   :: Double,
    f            :: Double, -- Focal distance
    zoom         :: Double, -- Zoom factor
    exposureTime :: Double
  }
  deriving Show


thinLens :: ThinLens -> Camera
thinLens = Camera 


instance Renderizer ThinLens where
  render ThinLens{..} = do
    vres <- getVRes
    hres <- getHRes
    let onb     = computeUVW eye lookat up
        rows    = [vres-1, vres-2..0]
        columns = [0..hres-1]
        pairs   = [(r, c) | r <- rows, c <- columns]
    traverse (processPixel eye onb d lensRadius f zoom) pairs



processPixel :: Eye
             -> ONB
             -> Double  -- Viewplane distance
             -> Double  -- Lens radius
             -> Double  -- Focal distance
             -> Double  -- Zoom
             -> (Int, Int)
             -> HR C.RGBColor
processPixel eye onb d lensRadius f zoom (r, c) = do
  when (c == 0) $ writeErrLn $ "Scanlines remaining: " ++ show r

  s <- (/zoom) <$> getPixelSize -- Dividido por factor zoom
  samples  <- zip <$> sampleUnitSquare <*> sampleUnitDisk
  rays     <- traverse (genRay eye onb d lensRadius f s (r, c)) samples
  tracer   <- getTracer
  colors   <- traverse tracer rays
  nsamples <- getNumSamples

  -- Se devuelve una computacion pura, forzamos su evaluacion
  return $! foldr1 C.add colors `C.divi` fromIntegral nsamples



genRay :: Eye
       -> ONB
       -> Double               -- View plane distance
       -> Double               -- Lens radius
       -> Double               -- Focal distance
       -> Double               -- Pixel size
       -> (Int, Int)           -- (row, column)
       -> (Sample2D, Sample2D) -- (Sample square, Sample disk)
       -> HR Ray
genRay eye onb@ONB{..} d lensRadius f s (r, c) (sp, dp) = do
  vres <- getVRes
  hres <- getHRes
  let pp  = point2D (s*(fromIntegral c - 0.5*fromIntegral hres + P.x sp))
                    (s*(fromIntegral r - 0.5*fromIntegral vres + P.y sp))
      lp  = dp `P.mul` lensRadius
      ori = eye `add` mul u (lp&P.x) `add` mul v (lp&P.y)
      dir = rayDirection onb d f pp lp
  return $ ray ori dir



rayDirection :: ONB
             -> Double   -- View plane distance
             -> Double   -- Focal distance
             -> Point2D  -- Pixel point
             -> Point2D  -- Lens point
             -> Vector3D
rayDirection ONB{..} d f pp lp = normalize dir
  where
    p   = point2D (P.x pp * f / d) (P.y pp * f /d)
    dir = mul u (P.x p-P.x lp) `add` mul v (P.y p-P.y lp) `diff` mul w f

