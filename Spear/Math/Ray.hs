module Spear.Math.Ray
(
    Ray(..)
,   raylr
,   rayfb
)
where


import Spear.Math.Utils
import Spear.Math.Vector


data Ray = Ray
    { origin :: {-# UNPACK #-} !Vector2
    , dir    :: {-# UNPACK #-} !Vector2
    }


-- | Classify the given point's position with respect to the given ray. Left/Right test.
raylr :: Ray -> Vector2 -> Side
raylr (Ray o d) p
    | orientation2d o (o+d) p < 0 = R
    | otherwise = L


-- | Classify the given point's position with respect to the given ray. Front/Back test.
rayfb :: Ray -> Vector2 -> Face
rayfb (Ray o d) p
    | orientation2d o (perp d) p > 0 = F
    | otherwise = B
