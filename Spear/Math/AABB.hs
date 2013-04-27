module Spear.Math.AABB
(
    AABB2(..)
,   AABB3(..)
,   aabb2
,   aabb3
,   aabb2pt
,   aabb3pt
)
where

import Spear.Math.Vector

import Data.List (foldl')

-- | An axis-aligned bounding box in 2D space.
data AABB2 = AABB2 {-# UNPACK #-} !Vector2 {-# UNPACK #-} !Vector2

-- | An axis-aligned bounding box in 3D space.
data AABB3 = AABB3 {-# UNPACK #-} !Vector3 {-# UNPACK #-} !Vector3

-- | Create a AABB from the given points.
aabb2 :: [Vector2] -> AABB2
aabb2 [] = AABB2 zero2 zero2
aabb2 (x:xs) = foldl' update (AABB2 x x) xs
      where update (AABB2 pmin pmax) p = AABB2 (min p pmin) (max p pmax)

-- | Create an AABB from the given points.
aabb3 :: [Vector3] -> AABB3
aabb3 [] = AABB3 zero3 zero3
aabb3 (x:xs) = foldl' update (AABB3 x x) xs
      where update (AABB3 pmin pmax) p = AABB3 (min p pmin) (max p pmax)

-- | Return 'True' if the given AABB contains the given point, 'False' otherwise.
aabb2pt :: AABB2 -> Vector2 -> Bool
aabb2pt (AABB2 pmin pmax) v = v >= pmin && v <= pmax

-- | Return 'True' if the given AABB contains the given point, 'False' otherwise.
aabb3pt :: AABB3 -> Vector3 -> Bool
aabb3pt (AABB3 pmin pmax) v = v >= pmin && v <= pmax
