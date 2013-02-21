module Spear.Math.AABB
(
    AABB(..)
,   aabb
,   aabbpt
)
where


import Spear.Math.Vector


-- | An axis-aligned bounding box.
data AABB = AABB {-# UNPACK #-} !Vector2 {-# UNPACK #-} !Vector2


-- | Create a 'AABB' from the given points.
aabb :: [Vector2] -> AABB

aabb [] = error "Attempting to build a BoundingVolume from an empty list!"

aabb (x:xs) = foldr update (AABB x x) xs
    where update p (AABB pmin pmax) = AABB (min p pmin) (max p pmax)


-- | Return 'True' if the given 'AABB' contains the given point, 'False' otherwise.         
aabbpt :: AABB -> Vector2 -> Bool
aabbpt (AABB pmin pmax) v = v >= pmin && v <= pmax
