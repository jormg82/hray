
module Main where

import Utility.Params
import World.World
  


main :: IO ()
main = build defaultParams >>= renderWorld
