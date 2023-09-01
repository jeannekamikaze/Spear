{-# LANGUAGE NoImplicitPrelude #-}

module Spear.Math.Utils
(
    Side(..)
,   Face(..)
,   orientation2d
,   viewToWorld2d
)
where

import           Spear.Math.Algebra
import           Spear.Math.Matrix4 as M4
import           Spear.Math.Vector  as V
import           Spear.Prelude


data Side = L | R deriving (Eq, Show)


data Face = F | B deriving (Eq, Show)


-- | Return the signed area of the triangle defined by the given points.
orientation2d :: Vector2 -> Vector2 -> Vector2 -> Float
orientation2d p q r = (x q - x p) * (y r - y p) - (y q - y p) * (x r - x p)


-- | Project the given point in view space onto the XZ plane in world space.
viewToWorld2d :: Vector2 -- ^ Point in view space
              -> Matrix4 -- ^ Inverse view matrix
              -> Vector2 -- ^ Projection of the given point
viewToWorld2d p viewI =
    let
        p1' = vec3 (x p) (y p) 0
        p1  = viewI `mulp` p1'
        p2  = p1 - M4.forward viewI
        lambda = (y p1 / (y p1 - y p2))
        p'  = p1 + lambda * (p2 - p1)
    in
        vec2 (x p') (-z p')
