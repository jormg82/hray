{-# LANGUAGE RecordWildCards #-}

module Camera.Function(Function(..), function) where

import HRay.HRay
import qualified Utility.RGBColor as C
import qualified Utility.Point2D as P
import Utility.Ray
import Utility.Utility
import Utility.Vector3D

import Control.Monad(forM, when)


-- OJO poner datos de la camara
data Function = Function
  {
    fun :: P.Point2D -> C.RGBColor
  }

function :: Function -> Camera
function = Camera


instance Renderizer Function where
  render Function{..} = do
    vres <- getVRes
    hres <- getHRes
    let rows    = [vres-1, vres-2..0]
        columns = [0..hres-1]
        pairs   = [(r, c) | r <- rows, c <- columns]
    forM pairs (processPixel fun)



processPixel :: (P.Point2D -> C.RGBColor)
             -> (Int, Int)
             -> HR C.RGBColor
processPixel f (r, c) = do
  when (c == 0) $ writeErrLn $ "Scanlines remaining: " ++ show r
  (hres, vres, s) <- (,,) <$> getHRes <*> getVRes <*> getPixelSize
  let x      = s * (fromIntegral c - 0.5*fromIntegral hres)
      y      = s * (fromIntegral r - 0.5*fromIntegral vres)
  colors <- map f <$> sampleUnitSquare
  nsamples <- getNumSamples
  -- Se devuelve una computacion pura, forzamos su evaluacion
  return $! foldr1 C.add colors `C.divi` fromIntegral nsamples

