{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

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

import           Spear.Math.Spatial
import           Spear.Math.Vector
import           Spear.Prelude

import           Data.List          (foldl')


-- | An axis-aligned bounding box in 2D space.
data AABB2 = AABB2 {-# UNPACK #-} !Vector2 {-# UNPACK #-} !Vector2 deriving Show

-- | An axis-aligned bounding box in 3D space.
data AABB3 = AABB3 {-# UNPACK #-} !Vector3 {-# UNPACK #-} !Vector3 deriving Show


instance Positional AABB2 Vector2 where
      setPosition p (AABB2 pmin pmax) = AABB2 p (p + (pmax - pmin))
      position (AABB2 pmin pmax) = pmin
      translate p (AABB2 pmin pmax) = AABB2 (p + pmin) (p + pmax)


instance Positional AABB3 Vector3 where
      setPosition p (AABB3 pmin pmax) = AABB3 p (p + (pmax - pmin))
      position (AABB3 pmin pmax) = pmin
      translate p (AABB3 pmin pmax) = AABB3 (p + pmin) (p + pmax)


-- | Create a AABB from the given points.
aabb2 :: [Vector2] -> AABB2
aabb2 = foldl' union (AABB2 zero2 zero2)
      where union (AABB2 pmin pmax) p = AABB2 (min p pmin) (max p pmax)

-- | Create an AABB from the given points.
aabb3 :: [Vector3] -> AABB3
aabb3 = foldl' union (AABB3 zero3 zero3)
      where union (AABB3 pmin pmax) p = AABB3 (min p pmin) (max p pmax)

-- | Return 'True' if the given AABB contains the given point, 'False' otherwise.
aabb2pt :: AABB2 -> Vector2 -> Bool
aabb2pt (AABB2 pmin pmax) v = v >= pmin && v <= pmax

-- | Return 'True' if the given AABB contains the given point, 'False' otherwise.
aabb3pt :: AABB3 -> Vector3 -> Bool
aabb3pt (AABB3 pmin pmax) v = v >= pmin && v <= pmax
