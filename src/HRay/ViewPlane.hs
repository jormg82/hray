

module HRay.ViewPlane(ViewPlane(..)) where


data ViewPlane = ViewPlane
  {
    hres  :: Int,    -- Horizontal resolution
    vres  :: Int,    -- Vertical resolution
    s     :: Double, -- Pixel size
    gamma :: Double  -- Gamma correction
  }
  deriving Show

