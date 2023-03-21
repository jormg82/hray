{-# LANGUAGE GADTs #-}

module HRay.HRay (
  Camera(..),
  Scene(..),
  Renderizer(..),
  Tracer,
  ViewPlane(..),
  HRState(..),
  HR,
  runHR,
  throw,
  try,
  writeStdLn,
  writeErrLn,
  getCamera,
  getNumSamples,
  sampleUnitSquare,
  sampleUnitDisk,
  sampleHemisphere,
  mapSamplesToHemisphere,
  getScene,
  getTracer,
  trace,
  getHRes,
  getVRes,
  getPixelSize, 
  getGamma
) where


import Object.Object
import qualified Sampler.Sampler as S
import Utility.RGBColor
import Utility.Ray

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except(runExceptT, ExceptT, throwE, catchE)
import Control.Monad.Trans.State(evalStateT, StateT, get, put)
import System.IO



-- Camera defs
class Renderizer a where
  render :: a -> HR [RGBColor]

data Camera where
  Camera :: Renderizer a => a -> Camera

instance Renderizer Camera where
  render (Camera r) = render r


-- Scene
data Scene = Scene {objects :: [Object], backgroundColor :: RGBColor}


-- Tracer
type Tracer = Ray -> HR RGBColor


-- ViewPlane
data ViewPlane = ViewPlane
  {
    hres  :: Int,    -- Horizontal resolution
    vres  :: Int,    -- Vertical resolution
    s     :: Double, -- Pixel size
    gamma :: Double  -- Gamma correction
  }
  deriving Show


-- State
data HRState = HRState
  {
    camera    :: Camera,
    sampler   :: S.Sampler,
    scene     :: Scene,
    tracer    :: Tracer,
    viewPlane :: ViewPlane
  }


type HR = ExceptT String (StateT HRState IO)


runHR :: HR a -> HRState -> IO (Either String a)
runHR = evalStateT . runExceptT
 


-- error
throw :: String -> HR a
throw = throwE


try :: HR a -> (String -> HR a) -> HR a
try = catchE


-- trazas
writeStdLn :: String -> HR ()
writeStdLn = liftIO . putStrLn

blanks :: String
blanks = "\r" ++ replicate 32 ' ' ++ "\r"

writeErrLn :: String -> HR ()
writeErrLn s = do
  liftIO $ hPutStr stderr blanks
  liftIO $ hPutStr stderr s



-- Camera access
getCamera :: HR Camera
getCamera = camera <$> lift get



-- Sampler
getNumSamples :: HR Int
getNumSamples = do
  s <- sampler <$> lift get
  return $ S.numSamples s


sampleUnitSquare :: HR [S.Sample2D]
sampleUnitSquare = do
  s <- sampler <$> lift get
  liftIO $ S.unitSquare s

sampleUnitDisk :: HR [S.Sample2D]
sampleUnitDisk = do
  s <- sampler <$> lift get
  liftIO $ S.unitDisk s

sampleHemisphere :: HR [S.Sample3D]
sampleHemisphere = do
  s <- sampler <$> lift get
  liftIO $ S.hemisphere s


mapSamplesToHemisphere :: Double -> HR ()
mapSamplesToHemisphere e = do
  s <- lift get
  lift $ put s{sampler=S.mapSamplesToHemisphere e $ sampler s}



-- Scene
getScene :: HR Scene
getScene = scene <$> lift get


-- Tracer
getTracer :: HR Tracer
getTracer = tracer <$> lift get

trace :: Ray -> HR RGBColor
trace ray = do
  tr <- tracer <$> lift get
  tr ray


-- Acceso a ViewPlane
getHRes :: HR Int
getHRes = (hres . viewPlane) <$> lift get

getVRes :: HR Int
getVRes = (vres . viewPlane) <$> lift get

getPixelSize :: HR Double
getPixelSize = (s . viewPlane) <$> lift get

getGamma :: HR Double
getGamma = (gamma . viewPlane) <$> lift get

