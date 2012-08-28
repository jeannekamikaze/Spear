module Spear.Math.AABB
(
    AABB(..)
,   aabb
,   aabbpt
)
where


import Spear.Math.Vector2


-- | An axis-aligned bounding box.
data AABB = AABB {-# UNPACK #-} !Vector2 {-# UNPACK #-} !Vector2


-- | Create a 'AABB' from the given points.
aabb :: [Vector2] -> AABB

aabb [] = error "Attempting to build a BoundingVolume from an empty list!"

aabb (x:xs) = foldr update (AABB x x) xs
    where update p (AABB min max) = AABB (v2min p min) (v2max p max)


-- | Return 'True' if the given 'AABB' contains the given point, 'False' otherwise.         
aabbpt :: AABB -> Vector2 -> Bool
aabbpt (AABB min max) v = v >= min && v <= max
