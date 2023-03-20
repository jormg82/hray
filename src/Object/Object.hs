{-# LANGUAGE GADTs #-}

module Object.Object (
  Object(..),
  Hittable(..),
) where

import HRay.ShadeRec
import Utility.Ray
import Utility.RGBColor

import Data.Maybe(catMaybes, listToMaybe)
import Data.List(sortOn)



class Hittable a where
  hit :: a -> Ray -> Maybe (Double, ShadeRec)

instance Hittable a => Hittable [a] where
  hit xs r = listToMaybe $ sortOn fst $ catMaybes $ map (flip hit r) xs


data Object where
  Object :: Hittable a => {object :: a} -> Object

instance Hittable Object where
  hit (Object o) = hit o
