{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Spear.Math.Vector.Vector2
(
    Vector2(..)
,   Right2
,   Up2
,   Position2
    -- * Construction
,   unitx2
,   unity2
,   zero2
,   vec2
    -- * Operations
,   perp
)
where

import           Spear.Math.Algebra
import           Spear.Math.Vector.Vector
import           Spear.Prelude

import           Foreign.C.Types          (CFloat)
import           Foreign.Storable
import qualified Prelude                  as P


type Right2    = Vector2
type Up2       = Vector2
type Position2 = Vector2


-- | Represents a vector in 2D.
data Vector2 = Vector2 {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving (Eq, Show)


instance Addition Vector2 Vector2 where
    {-# INLINABLE (+) #-}
    Vector2 ax ay + Vector2 bx by = Vector2 (ax + bx) (ay + by)


instance Subtraction Vector2 Vector2 where
    {-# INLINABLE (-) #-}
    Vector2 ax ay - Vector2 bx by = Vector2 (ax - bx) (ay - by)


instance Product Vector2 Vector2 Vector2 where
    {-# INLINABLE (*) #-}
    Vector2 ax ay * Vector2 bx by = Vector2 (ax * bx) (ay * by)


instance Quotient Vector2 Vector2 where
    {-# INLINABLE (/) #-}
    Vector2 ax ay / Vector2 bx by = Vector2 (ax / bx) (ay / by)


-- Scalar product.
instance Product Vector2 Float Vector2 where
    {-# INLINABLE (*) #-}
    (Vector2 x y) * s = Vector2 (s * x) (s * y)


instance Product Float Vector2 Vector2 where
    {-# INLINABLE (*) #-}
    s * (Vector2 x y) = Vector2 (s * x) (s * y)


-- Scalar division.
instance Quotient Vector2 Float where
    {-# INLINABLE (/) #-}
    (Vector2 x y) / s = Vector2 (x / s) (y / s)


instance Num Vector2 where
    (+) = add
    (-) = sub
    (*) = mul
    abs (Vector2 ax ay) = Vector2 (abs ax) (abs ay)
    signum (Vector2 ax ay) = Vector2 (signum ax) (signum ay)
    fromInteger i = Vector2 i' i' where i' = fromInteger i


instance Fractional Vector2 where
    (/) = Spear.Math.Algebra.div
    fromRational r = Vector2 r' r' where r' = fromRational r


instance Ord Vector2 where
    Vector2 ax ay <= Vector2 bx by =  (ax <= bx) || (ax == bx && ay <= by)
    Vector2 ax ay >= Vector2 bx by =  (ax >= bx) || (ax == bx && ay >= by)
    Vector2 ax ay < Vector2 bx by  =  (ax < bx)  || (ax == bx && ay < by)
    Vector2 ax ay > Vector2 bx by  =  (ax > bx)  || (ax == bx && ay > by)
    max (Vector2 ax ay) (Vector2 bx by) = Vector2 (max ax bx) (max ay by)
    min (Vector2 ax ay) (Vector2 bx by) = Vector2 (min ax bx) (min ay by)


instance Vector Vector2 where
    {-# INLINABLE fromList #-}
    fromList (ax:ay:_) = Vector2 ax ay

    {-# INLINABLE x #-}
    x (Vector2 ax _) = ax

    {-# INLINABLE y #-}
    y (Vector2 _ ay) = ay

    {-# INLINABLE (!) #-}
    (Vector2 ax _) ! 0 = ax
    (Vector2 _ ay) ! 1 = ay
    _              ! _ = 0

    {-# INLINABLE dot #-}
    Vector2 ax ay `dot` Vector2 bx by = ax*bx + ay*by

    {-# INLINABLE normSq #-}
    normSq (Vector2 ax ay) = ax*ax + ay*ay

    {-# INLINABLE norm #-}
    norm = sqrt . normSq

    {-# INLINABLE neg #-}
    neg (Vector2 ax ay) = Vector2 (-ax) (-ay)

    {-# INLINABLE normalise #-}
    normalise v =
            let n' = norm v
                n = if n' == 0 then 1 else n'
            in ((1.0::Float) / n) * v


sizeFloat = sizeOf (undefined :: CFloat)


instance Storable Vector2 where
    sizeOf _    = (2::Int) * sizeFloat
    alignment _ = alignment (undefined :: CFloat)

    peek ptr = do
        ax <- peekByteOff ptr 0
        ay <- peekByteOff ptr $ sizeFloat
        return (Vector2 ax ay)

    poke ptr (Vector2 ax ay) = do
        pokeByteOff ptr 0 ax
        pokeByteOff ptr sizeFloat ay


-- | Unit vector along the X axis.
unitx2 = Vector2 1 0

-- | Unit vector along the Y axis.
unity2 = Vector2 0 1

-- | Zero vector.
zero2 = Vector2 0 0

-- | Create a vector from the given values.
vec2 :: Float -> Float -> Vector2
vec2 = Vector2

-- | Compute a perpendicular vector satisfying:
--
-- perp (Vector2 0 1) = Vector2 1 0
--
-- perp (Vector2 1 0) = Vector2 0 (-1)
perp :: Vector2 -> Vector2
perp (Vector2 x y) = Vector2 y (-x)
