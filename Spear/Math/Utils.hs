module Spear.Math.Utils
(
    Side(..)
,   Face(..)
,   orientation2d
,   viewToWorld2d
)
where


import Spear.Math.Matrix4 as M4
import Spear.Math.Vector2 as V2
import qualified Spear.Math.Vector3 as V3


data Side = L | R deriving (Eq, Show)


data Face = F | B deriving (Eq, Show)


-- | Return the signed area of the triangle defined by the given points.
orientation2d :: Vector2 -> Vector2 -> Vector2 -> Float
orientation2d p q r = (x q - x p) * (y r - y p) - (y q - y p) * (x r - x p)


-- | Project the given point in view space onto the XZ plane in world space.
viewToWorld2d :: Vector2
              -> Matrix4
              -> Vector2
viewToWorld2d p viewI =
    let
        p1' = V3.vec3 (V2.x p) (V2.y p) 0
        p1  = viewI `mulp` p1'
        p2  = p1 - M4.forward viewI
        lambda = (V3.y p1 / (V3.y p1 - V3.y p2))
        p'  = p1 + V3.scale lambda (p2 - p1)
    in
        vec2 (V3.x p') (-V3.z p')
