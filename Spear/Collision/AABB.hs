module Spear.Collision.AABB
(
    AABB(..)
,   aabb
,   contains
)
where


import Spear.Math.Vector3 as Vector


-- | An axis-aligned bounding box.
data AABB = AABB
    { min :: !Vector3
    , max :: !Vector3
    }
    deriving Eq


-- | Create a 'AABB' from the given points.
aabb :: [Vector3] -> AABB

aabb [] = error "Attempting to build a BoundingVolume from an empty list!"

aabb (x:xs) = foldr update (AABB x x) xs
    where update p (AABB min max) = AABB (Vector.min p min) (Vector.max p max)


-- | Return 'True' if the given 'AABB' contains the given point, 'False' otherwise.         
contains :: AABB -> Vector3 -> Bool
(AABB min max) `contains` v = v >= min && v <= max
