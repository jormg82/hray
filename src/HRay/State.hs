
module HRay.State (
  HRState(..),
  HR,
  runHR,
  throw,
  try,
  writeStdLn,
  writeErrLn,
  getNumSamples,
  sampleUnitSquare,
  sampleUnitDisk,
  sampleHemisphere,
  mapSamplesToHemisphere,
  getScene,
  getHRes,
  getVRes,
  getPixelSize, 
  getGamma
) where


import HRay.Scene
import HRay.ViewPlane
import qualified Sampler.Sampler as S
import Utility.RGBColor
import Utility.Ray

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except(runExceptT, ExceptT, throwE, catchE)
import Control.Monad.Trans.State(evalStateT, StateT, get, put)
import System.IO



data HRState = HRState
  {
    sampler   :: S.Sampler,
    scene     :: Scene,
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
getScene = lift get >>= return . scene


-- Acceso a ViewPlane
getHRes :: HR Int
getHRes = (hres . viewPlane) <$> lift get

getVRes :: HR Int
getVRes = (vres . viewPlane) <$> lift get

getPixelSize :: HR Double
getPixelSize = (s . viewPlane) <$> lift get

getGamma :: HR Double
getGamma = (gamma . viewPlane) <$> lift get

