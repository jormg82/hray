
module Tracer.Tracer (Tracer) where

import HRay.State
import Utility.Ray
import Utility.RGBColor


type Tracer = Ray -> HR RGBColor
