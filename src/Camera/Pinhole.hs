{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Camera.Pinhole(Pinhole(..), pinhole) where

import HRay.HRay
import Sampler.Sampler
import qualified Utility.RGBColor as C
import qualified Utility.Point2D as P
import Utility.Ray
import Utility.Utility
import Utility.Vector3D

import Control.Monad(when)


data Pinhole = Pinhole
  {
    eye    :: Eye,
    lookat :: LookAt,
    up     :: Up,
    d      :: Double, -- ViewPlane distance
    zoom   :: Double, -- Zoom factor
    exposureTime :: Double
  }
  deriving Show


pinhole :: Pinhole -> Camera
pinhole = Camera 


instance Renderizer Pinhole where
  render Pinhole{..} = do
    vres <- getVRes
    hres <- getHRes
    let onb     = computeUVW eye lookat up
        rows    = [vres-1, vres-2..0]
        columns = [0..hres-1]
        pairs   = [(r, c) | r <- rows, c <- columns]
    traverse (processPixel eye d zoom onb) pairs



processPixel :: Eye
             -> Double  -- Viewplane distance
             -> Double  -- Zoom
             -> ONB
             -> (Int, Int)
             -> HR C.RGBColor
processPixel eye d zoom onb (r, c) = do
  when (c == 0) $ writeErrLn $ "Scanlines remaining: " ++ show r

  vres <- getVRes
  hres <- getHRes
  s    <- (*zoom) <$> getPixelSize -- Multiplicado por factor zoom

  samples  <- sampleUnitSquare
  let rays = map (ray eye . genDir onb d vres hres s (r, c)) samples
  tracer   <- getTracer
  colors   <- traverse tracer rays
  nsamples <- getNumSamples

  -- Se devuelve una computacion pura, forzamos su evaluacion
  return $! foldr1 C.add colors `C.divi` fromIntegral nsamples



genDir :: ONB        -- Orthonormal base
       -> Double     -- View plane distance
       -> Int        -- Vertical resolution
       -> Int        -- Horizontal resolution
       -> Double     -- Pixel size
       -> (Int, Int) -- (row, column)
       -> Sample2D   -- Sample in [0, 1]^2
       -> Vector3D
genDir ONB{..} d vres hres s (r, c) sample =
  normalize $ mul u x `add` mul v y `diff` mul w d
  where
    x = s * (fromIntegral c - 0.5*fromIntegral hres + P.x sample)
    y = s * (fromIntegral r - 0.5*fromIntegral vres + P.y sample)

