
module Utility.Constants (
  twopi, invpi, invtwopi,
  pion180,
  inf, epsilon
) where


twopi, invpi, invtwopi :: Double
twopi    = 2 * pi
invpi    = 1.0 / pi
invtwopi = 1.0 / twopi

pion180 :: Double
pion180 = pi / 180.0

inf :: Double
inf = 1.0 / 0.0

epsilon :: Double
epsilon = 0.0001
