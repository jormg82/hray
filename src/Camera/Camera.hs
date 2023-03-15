
module Camera.Camera (
  Eye, LookAt, Up,
  Camera,
  computeUVW
) where


import HRay.State
import Tracer.Tracer
import Utility.RGBColor
import qualified Utility.Vector3D as V


type Eye    = V.Point3D
type LookAt = V.Point3D
type Up     = V.Vector3D


type Camera = Tracer -> HR [RGBColor]


computeUVW :: Eye -> LookAt -> Up -> V.ONB
computeUVW eye lookat up = V.ONB{V.u=u, V.v=v, V.w=w}
  where
    w = V.normalize $ eye `V.diff` lookat
    u = V.normalize $ up `V.cross` w
    v = w `V.cross` u
