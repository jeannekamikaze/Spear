{-# LANGUAGE NoImplicitPrelude #-}

module Spear.Math.Plane
(
    Plane
,   plane
,   classify
)
where

import           Spear.Math.Vector
import           Spear.Prelude

data PointPlanePos = Front | Back | Contained deriving (Eq, Show)

data Plane = Plane
    { n :: {-# UNPACK #-} !Vector3,
      d :: {-# UNPACK #-} !Float
    }
    deriving(Eq, Show)

-- | Construct a plane from  a normal vector and a distance from the origin.
plane :: Vector3 -> Float -> Plane
plane n d = Plane (normalise n) d

-- | Construct a plane from three points.
--
-- Points must be given in counter-clockwise order.
fromPoints :: Vector3 -> Vector3 -> Vector3 -> Plane
fromPoints p0 p1 p2 = Plane n d
           where n  = normalise $ v1 `cross` v2
                 v1 = p2 - p1
                 v2 = p0 - p1
                 d  = p0 `dot` n

-- | Classify the given point's relative position with respect to the plane.
classify :: Plane -> Vector3 -> PointPlanePos
classify (Plane n d) pt =
    case (n `dot` pt - d) `compare` 0 of
        GT -> Front
        LT -> Back
        EQ -> Contained
