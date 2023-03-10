
module Object.Object (
  HitObjectFn,
  Object(),
  object,
  Hit(..),
) where

import Utility.Ray
import Utility.RGBColor
import Utility.ShadeRec


import Data.Maybe(catMaybes, listToMaybe)
import Data.List(sortOn)


type HitObjectFn = Ray -> Maybe (Double, ShadeRec)

newtype Object = Object HitObjectFn

object :: HitObjectFn -> Object
object = Object


class Hit a where
  hit :: Ray -> a -> Maybe (Double, ShadeRec)

instance Hit Object where
  hit r (Object f) = f r

instance Hit a => Hit [a] where
  hit r = listToMaybe . sortOn fst . catMaybes . map (hit r)

