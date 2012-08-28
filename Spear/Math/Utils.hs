module Spear.Math.Utils
(
    Side(..)
,   Face(..)
,   orientation2d
)
where


import Spear.Math.Vector2


data Side = L | R deriving (Eq, Show)


data Face = F | B deriving (Eq, Show)


-- | Return the signed area of the triangle defined by the given points.
orientation2d :: Vector2 -> Vector2 -> Vector2 -> Float
orientation2d p q r = (x q - x p) * (y r - y p) - (y q - y p) * (x r - x p)
