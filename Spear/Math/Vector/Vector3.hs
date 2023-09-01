{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Spear.Math.Vector.Vector3
(
    Vector3(..)
,   Right3
,   Up3
,   Forward3
,   Position3
,   sizeVector3
    -- * Construction
,   unitx3
,   unity3
,   unitz3
,   zero3
,   vec3
,   orbit
    -- * Operations
,   cross
)
where

import           Spear.Math.Algebra
import           Spear.Math.Vector.Vector
import           Spear.Prelude

import           Foreign.C.Types          (CFloat)
import           Foreign.Storable
import qualified Prelude                  as P

type Right3    = Vector3
type Up3       = Vector3
type Forward3  = Vector3
type Position3 = Vector3


-- | Represents a vector in 3D.
data Vector3 = Vector3
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    deriving (Eq, Show)


sizeVector3 = (3::Int) * sizeOf (undefined :: CFloat)


instance Addition Vector3 Vector3 where
    {-# INLINABLE (+) #-}
    Vector3 ax ay az + Vector3 bx by bz = Vector3 (ax + bx) (ay + by) (az + bz)


instance Subtraction Vector3 Vector3 where
    {-# INLINABLE (-) #-}
    Vector3 ax ay az - Vector3 bx by bz = Vector3 (ax - bx) (ay - by) (az - bz)


instance Product Vector3 Vector3 Vector3 where
    {-# INLINABLE (*) #-}
    Vector3 ax ay az * Vector3 bx by bz = Vector3 (ax * bx) (ay * by) (az * bz)


instance Quotient Vector3 Vector3 where
    {-# INLINABLE (/) #-}
    Vector3 ax ay az / Vector3 bx by bz = Vector3 (ax / bx) (ay / by) (az / bz)


-- Scalar product.
instance Product Vector3 Float Vector3 where
    {-# INLINABLE (*) #-}
    (Vector3 x y z) * s = Vector3 (s * x) (s * y) (s * z)


instance Product Float Vector3 Vector3 where
    {-# INLINABLE (*) #-}
    s * (Vector3 x y z) = Vector3 (s * x) (s * y) (s * z)


-- Scalar division.
instance Quotient Vector3 Float where
    {-# INLINABLE (/) #-}
    (Vector3 x y z) / s = Vector3 (x / s) (y / s) (y / s)


instance Num Vector3 where
    (+) = add
    (-) = sub
    (*) = mul
    abs (Vector3 ax ay az) = Vector3 (abs ax) (abs ay) (abs az)
    signum (Vector3 ax ay az) = Vector3 (signum ax) (signum ay) (signum az)
    fromInteger i = Vector3 i' i' i' where i' = fromInteger i


instance Fractional Vector3 where
    (/) = Spear.Math.Algebra.div
    fromRational r = Vector3 r' r' r' where r' = fromRational r


instance Ord Vector3 where
    Vector3 ax ay az <= Vector3 bx by bz
        =  (ax <= bx)
        || (az == bx && ay <= by)
        || (ax == bx && ay == by && az <= bz)

    Vector3 ax ay az >= Vector3 bx by bz
        =  (ax >= bx)
        || (ax == bx && ay >= by)
        || (ax == bx && ay == by && az >= bz)

    Vector3 ax ay az < Vector3 bx by bz
        =  (ax < bx)
        || (az == bx && ay < by)
        || (ax == bx && ay == by && az < bz)

    Vector3 ax ay az > Vector3 bx by bz
        =  (ax > bx)
        || (ax == bx && ay > by)
        || (ax == bx && ay == by && az > bz)

    max (Vector3 ax ay az) (Vector3 bx by bz) =
        Vector3 (max ax bx) (max ay by) (max az bz)

    min (Vector3 ax ay az) (Vector3 bx by bz) =
        Vector3 (min ax bx) (min ay by) (min az bz)


instance Vector Vector3 where
    {-# INLINABLE fromList #-}
    fromList (ax:ay:az:_) = Vector3 ax ay az

    {-# INLINABLE x #-}
    x (Vector3 ax _  _ ) = ax

    {-# INLINABLE y #-}
    y (Vector3 _  ay _ ) = ay

    {-# INLINABLE z #-}
    z (Vector3 _  _  az) = az

    {-# INLINABLE (!) #-}
    (Vector3 ax _ _) ! 0 = ax
    (Vector3 _ ay _) ! 1 = ay
    (Vector3 _ _ az) ! 2 = az
    _                ! _ = 0

    {-# INLINABLE dot #-}
    Vector3 ax ay az `dot` Vector3 bx by bz = ax*bx + ay*by + az*bz

    {-# INLINABLE normSq #-}
    normSq (Vector3 ax ay az) = ax*ax + ay*ay + az*az

    {-# INLINABLE norm #-}
    norm = sqrt . normSq

    {-# INLINABLE neg #-}
    neg (Vector3 ax ay az) = Vector3 (-ax) (-ay) (-az)

    {-# INLINABLE normalise #-}
    normalise v =
            let n' = norm v
                n = if n' == 0 then 1 else n'
            in ((1.0::Float) / n) * v


sizeFloat = sizeOf (undefined :: CFloat)


instance Storable Vector3 where
    sizeOf _    = (3::Int) * sizeFloat
    alignment _ = alignment (undefined :: CFloat)

    peek ptr = do
        ax <- peekByteOff ptr 0
        ay <- peekByteOff ptr $ (1::Int) * sizeFloat
        az <- peekByteOff ptr $ (2::Int) * sizeFloat
        return (Vector3 ax ay az)

    poke ptr (Vector3 ax ay az) = do
        pokeByteOff ptr 0 ax
        pokeByteOff ptr ((1::Int) * sizeFloat) ay
        pokeByteOff ptr ((2::Int) * sizeFloat) az


-- | Unit vector along the X axis.
unitx3 = Vector3 1 0 0

-- | Unit vector along the Y axis.
unity3 = Vector3 0 1 0

-- | Unit vector along the Z axis.
unitz3 = Vector3 0 0 1

-- | Zero vector.
zero3 = Vector3 0 0 0

-- | Create a 3D vector from the given values.
vec3 :: Float -> Float -> Float -> Vector3
vec3 = Vector3

-- | Create a 3D vector as a point on a sphere.
orbit :: Vector3 -- ^ Sphere center.
      -> Float -- ^ Sphere radius
      -> Float -- ^ Azimuth angle.
      -> Float -- ^ Zenith angle.
      -> Vector3
orbit center radius anglex angley =
    let sx = sin anglex
        sy = sin angley
        cx = cos anglex
        cy = cos angley
        px = x center + radius*cy*sx
        py = y center + radius*sy
        pz = z center + radius*cx*cy
    in
        vec3 px py pz

-- | Compute the given vectors' cross product.
cross :: Vector3 -> Vector3 -> Vector3
(Vector3 ax ay az) `cross` (Vector3 bx by bz) =
    Vector3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
