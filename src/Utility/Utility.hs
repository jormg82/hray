
module Utility.Utility (
  degreesToRadians,
  clamp, (&),
  randomDouble,
  randomShuffle,
  messageStdErr
) where

import Utility.Constants
import Data.Function((&))

import System.Console.ANSI
import System.IO
import System.Random


degreesToRadians :: Double -> Double
degreesToRadians = (*pion180)


clamp :: Double -- Value
      -> Double -- Min
      -> Double -- Max
      -> Double
clamp val m n | val < m   = m
              | val > n   = n
              | otherwise = val


-- Devuelve un valor en [0, 1]
randomDouble :: IO Double
randomDouble = do
  a <- randomRIO (0, maxBound) :: IO Int
  return $ fromIntegral a / fromIntegral (maxBound :: Int)


randomShuffle :: [a] -> IO [a]
randomShuffle []       = return []
randomShuffle xs@(_:_) = do
  let maxElem = length xs - 1
  rpos <- randomRIO (0, maxElem)     
  let (xs', y:ys') = splitAt rpos xs
  (y:) <$> randomShuffle (xs'++ys')


messageStdErr :: String -> IO ()
messageStdErr s = do
  hClearLine stderr
  hPutStrLn stderr s
  hCursorUpLine stderr 1
