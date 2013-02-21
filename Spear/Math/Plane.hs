module Spear.Math.Plane
(
    Plane
,   plane
,   classify
)
where


import Spear.Math.Vector3


data PointPlanePos = Front | Back | Contained deriving (Eq, Ord, Show)


data Plane = Plane
    { n :: {-# UNPACK #-} !Vector3,
      d :: {-# UNPACK #-} !Float
    }
    deriving(Eq, Show)


-- | Create a plane given a normal vector and a distance from the origin.
plane :: Vector3 -> Float -> Plane
plane n d = Plane (normalise n) d


-- | Classify the given point's relative position with respect to the given plane.
classify :: Plane -> Vector3 -> PointPlanePos
classify (Plane n d) pt =
    case (n `dot` pt - d) `compare` 0 of
        GT -> Front
        LT -> Back
        EQ -> Contained
