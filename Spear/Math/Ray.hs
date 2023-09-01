{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Spear.Math.Ray
(
    Ray(..)
,   raylr
,   rayfb
)
where


import qualified Spear.Math.Matrix3  as Matrix3
import           Spear.Math.Spatial
import           Spear.Math.Spatial2
import           Spear.Math.Utils
import           Spear.Math.Vector
import           Spear.Prelude


data Ray = Ray
    { origin :: {-# UNPACK #-} !Vector2
    , dir    :: {-# UNPACK #-} !Vector2
    }


instance Positional Ray Vector2 where
    setPosition p ray = ray { origin = p }
    position = origin
    translate v ray = ray { origin = origin ray + v }


instance Rotational Ray Vector2 Angle where
    setRotation angle ray = ray { dir = setRotation angle (dir ray) }
    rotation = rotation . dir
    rotate angle ray = ray { dir = rotate angle (dir ray) }
    right = right . dir
    up = up . dir
    forward = forward . dir
    setForward forward ray = ray { dir = forward }


instance Spatial Ray Vector2 Angle Transform2 where
    setTransform (Transform2 matrix) ray =
        ray { origin = Matrix3.position matrix, dir = Matrix3.up matrix }
    transform ray =
        Transform2 $ Matrix3.transform (perp $ dir ray) (dir ray) (origin ray)


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
