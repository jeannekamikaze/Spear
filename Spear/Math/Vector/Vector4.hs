{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Spear.Math.Vector.Vector4
(
    Vector4(..)
    -- * Construction
,   unitx4
,   unity4
,   unitz4
,   vec4
    -- * Operations
,   cross'
)
where

import           Spear.Math.Algebra
import           Spear.Math.Vector.Vector
import           Spear.Prelude

import           Foreign.C.Types          (CFloat)
import           Foreign.Storable
import qualified Prelude                  as P


-- | Represents a vector in 3D.
data Vector4 = Vector4
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    deriving (Eq, Show)


instance Addition Vector4 Vector4 where
    {-# INLINABLE (+) #-}
    Vector4 ax ay az aw + Vector4 bx by bz bw =
        Vector4 (ax + bx) (ay + by) (az + bz) (aw + bw)


instance Subtraction Vector4 Vector4 where
    {-# INLINABLE (-) #-}
    Vector4 ax ay az aw - Vector4 bx by bz bw =
        Vector4 (ax - bx) (ay - by) (az - bz) (aw - bw)


instance Product Vector4 Vector4 Vector4 where
    {-# INLINABLE (*) #-}
    Vector4 ax ay az aw * Vector4 bx by bz bw =
        Vector4 (ax * bx) (ay * by) (az * bz) (aw * bw)


instance Quotient Vector4 Vector4 where
    {-# INLINABLE (/) #-}
    Vector4 ax ay az aw / Vector4 bx by bz bw =
        Vector4 (ax / bx) (ay / by) (az / bz) (aw / bw)


-- Scalar product.
instance Product Vector4 Float Vector4 where
    {-# INLINABLE (*) #-}
    (Vector4 x y z w) * s = Vector4 (s * x) (s * y) (s * z) (s * w)


instance Product Float Vector4 Vector4 where
    {-# INLINABLE (*) #-}
    s * (Vector4 x y z w) = Vector4 (s * x) (s * y) (s * z) (s * w)


-- Scalar division.
instance Quotient Vector4 Float where
    {-# INLINABLE (/) #-}
    (Vector4 x y z w) / s = Vector4 (x / s) (y / s) (y / s) (w / s)


instance Num Vector4 where
    (+) = add
    (-) = sub
    (*) = mul
    abs (Vector4 ax ay az aw) = Vector4 (abs ax) (abs ay) (abs az) (abs aw)
    signum (Vector4 ax ay az aw) = Vector4 (signum ax) (signum ay) (signum az) (signum aw)
    fromInteger i = Vector4 i' i' i' i' where i' = fromInteger i


instance Fractional Vector4 where
    (/) = Spear.Math.Algebra.div
    fromRational r = Vector4 r' r' r' r' where r' = fromRational r


instance Ord Vector4 where
    Vector4 ax ay az aw <= Vector4 bx by bz bw
        =  (ax <= bx)
        || (az == bx && ay <= by)
        || (ax == bx && ay == by && az <= bz)
        || (ax == bx && ay == by && az == bz && aw <= bw)

    Vector4 ax ay az aw >= Vector4 bx by bz bw
        =  (ax >= bx)
        || (ax == bx && ay >= by)
        || (ax == bx && ay == by && az >= bz)
        || (ax == bx && ay == by && az == bz && aw >= bw)

    Vector4 ax ay az aw < Vector4 bx by bz bw
        =  (ax < bx)
        || (az == bx && ay < by)
        || (ax == bx && ay == by && az < bz)
        || (ax == bx && ay == by && az == bz && aw < bw)

    Vector4 ax ay az aw > Vector4 bx by bz bw
        =  (ax > bx)
        || (ax == bx && ay > by)
        || (ax == bx && ay == by && az > bz)
        || (ax == bx && ay == by && az == bz && aw > bw)

    min (Vector4 ax ay az aw) (Vector4 bx by bz bw) =
        Vector4 (min ax bx) (min ay by) (min az bz) (min aw bw)

    max (Vector4 ax ay az aw) (Vector4 bx by bz bw) =
        Vector4 (max ax bx) (max ay by) (max az bz) (min aw bw)


instance Vector Vector4 where
    {-# INLINABLE fromList #-}
    fromList (ax:ay:az:aw:_) = Vector4 ax ay az aw

    {-# INLINABLE x #-}
    x (Vector4 ax _  _  _ ) = ax

    {-# INLINABLE y #-}
    y (Vector4 _  ay _  _ ) = ay

    {-# INLINABLE z #-}
    z (Vector4 _  _  az _ ) = az

    {-# INLINABLE w #-}
    w (Vector4 _  _  _  aw) = aw

    {-# INLINABLE (!) #-}
    (Vector4 ax _ _ _) ! 0 = ax
    (Vector4 _ ay _ _) ! 1 = ay
    (Vector4 _ _ az _) ! 2 = az
    (Vector4 _ _ _ aw) ! 3 = aw
    _                  ! _ = 0

    {-# INLINABLE dot #-}
    Vector4 ax ay az aw `dot` Vector4 bx by bz bw = ax*bx + ay*by + az*bz + aw*bw

    {-# INLINABLE normSq #-}
    normSq (Vector4 ax ay az aw) = ax*ax + ay*ay + az*az + aw*aw

    {-# INLINABLE norm #-}
    norm = sqrt . normSq

    {-# INLINABLE neg #-}
    neg (Vector4 ax ay az aw) = Vector4 (-ax) (-ay) (-az) (-aw)

    {-# INLINABLE normalise #-}
    normalise v =
            let n' = norm v
                n  = if n' == 0 then 1 else n'
            in ((1.0::Float) / n) * v


sizeFloat = sizeOf (undefined :: CFloat)


instance Storable Vector4 where
    sizeOf _    = (4::Int) * sizeFloat
    alignment _ = alignment (undefined :: CFloat)

    peek ptr = do
        ax <- peekByteOff ptr 0
        ay <- peekByteOff ptr $ (1::Int) * sizeFloat
        az <- peekByteOff ptr $ (2::Int) * sizeFloat
        aw <- peekByteOff ptr $ (3::Int) * sizeFloat
        return (Vector4 ax ay az aw)

    poke ptr (Vector4 ax ay az aw) = do
        pokeByteOff ptr 0 ax
        pokeByteOff ptr ((1::Int) * sizeFloat) ay
        pokeByteOff ptr ((2::Int) * sizeFloat) az
        pokeByteOff ptr ((3::Int) * sizeFloat) aw


-- | Unit vector along the X axis.
unitx4 = Vector4 1 0 0 0

-- | Unit vector along the Y axis.
unity4 = Vector4 0 1 0 0

-- | Unit vector along the Z axis.
unitz4 = Vector4 0 0 1 0

-- | Unit vector along the W axis.
unitw4 = Vector4 0 0 0 1

-- | Create a 4D vector from the given values.
vec4 :: Float -> Float -> Float -> Float -> Vector4
vec4 = Vector4

-- | Compute the given vectors' cross product.
-- The vectors are projected to 3D space. The resulting vector is the cross product of the vectors' projections with w=0.
cross' :: Vector4 -> Vector4 -> Vector4
(Vector4 ax ay az _) `cross'` (Vector4 bx by bz _) =
         Vector4 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx) 0
