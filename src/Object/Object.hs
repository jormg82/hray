
module Object.Object (
  Hit,
  Object(..),
  Hittable(..),
) where

import HRay.ShadeRec
import Utility.Ray
import Utility.RGBColor

import Data.Maybe(catMaybes, listToMaybe)
import Data.List(sortOn)


type Hit = Ray -> Maybe (Double, ShadeRec)

newtype Object = Object Hit


class Hittable a where
  hit :: a -> Hit

instance Hittable Object where
  hit (Object f) = f

instance Hittable a => Hittable [a] where
  hit xs r = listToMaybe $ sortOn fst $ catMaybes $ map (flip hit r) xs

