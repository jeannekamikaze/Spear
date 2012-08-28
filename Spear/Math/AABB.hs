module Spear.Math.AABB
(
    AABB(..)
,   aabb
,   aabbpt
)
where


import Spear.Math.Vector3 as Vector


-- | An axis-aligned bounding box.
data AABB = AABB {-# UNPACK #-} !Vector3 {-# UNPACK #-} !Vector3


-- | Create a 'AABB' from the given points.
aabb :: [Vector3] -> AABB

aabb [] = error "Attempting to build a BoundingVolume from an empty list!"

aabb (x:xs) = foldr update (AABB x x) xs
    where update p (AABB min max) = AABB (Vector.min p min) (Vector.max p max)


-- | Return 'True' if the given 'AABB' contains the given point, 'False' otherwise.         
aabbpt :: AABB -> Vector3 -> Bool
(AABB min max) `aabbpt` v = v >= min && v <= max
