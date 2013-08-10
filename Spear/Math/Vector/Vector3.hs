module Spear.Math.Vector.Vector3
(
    Vector3(..)
,   Right3
,   Up3
,   Forward3
,   Position3
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


import Spear.Math.Vector.Class

import Foreign.C.Types (CFloat)
import Foreign.Storable

type Right3 = Vector3
type Up3 = Vector3
type Forward3 = Vector3
type Position3 = Vector3


-- | Represents a vector in 3D.
data Vector3 = Vector3
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    deriving (Eq, Show)

instance Num Vector3 where
    Vector3 ax ay az + Vector3 bx by bz = Vector3 (ax + bx) (ay + by) (az + bz)
    Vector3 ax ay az - Vector3 bx by bz = Vector3 (ax - bx) (ay - by) (az - bz)
    Vector3 ax ay az * Vector3 bx by bz = Vector3 (ax * bx) (ay * by) (az * bz)
    abs (Vector3 ax ay az) = Vector3 (abs ax) (abs ay) (abs az)
    signum (Vector3 ax ay az) = Vector3 (signum ax) (signum ay) (signum az)
    fromInteger i = Vector3 i' i' i' where i' = fromInteger i


instance Fractional Vector3 where
    Vector3 ax ay az / Vector3 bx by bz = Vector3 (ax / bx) (ay / by) (az / bz)
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

    max (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (Prelude.max ax bx) (Prelude.max ay by) (Prelude.max az bz)

    min (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (Prelude.min ax bx) (Prelude.min ay by) (Prelude.min az bz)


instance VectorClass Vector3 where
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

         {-# INLINABLE scale #-}
         scale s (Vector3 ax ay az) = Vector3 (s*ax) (s*ay) (s*az)

         {-# INLINABLE neg #-}
         neg (Vector3 ax ay az) = Vector3 (-ax) (-ay) (-az)

         {-# INLINABLE normalise #-}
         normalise v =
                   let n' = norm v
                       n = if n' == 0 then 1 else n'
                   in scale (1.0 / n) v


sizeFloat = sizeOf (undefined :: CFloat)


instance Storable Vector3 where
    sizeOf _    = 3*sizeFloat
    alignment _ = alignment (undefined :: CFloat)

    peek ptr = do
        ax <- peekByteOff ptr 0
        ay <- peekByteOff ptr $ 1*sizeFloat
        az <- peekByteOff ptr $ 2*sizeFloat
        return (Vector3 ax ay az)

    poke ptr (Vector3 ax ay az) = do
        pokeByteOff ptr 0 ax
        pokeByteOff ptr (1*sizeFloat) ay
        pokeByteOff ptr (2*sizeFloat) az


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
vec3 ax ay az = Vector3 ax ay az


-- | Create a 3D vector as a point on a sphere.
orbit :: Vector3 -- ^ Sphere center.
      -> Float -- ^ Sphere radius
      -> Float -- ^ Azimuth angle.
      -> Float -- ^ Zenith angle.
      -> Vector3

orbit center radius anglex angley =
    let ax = anglex * pi / 180
        ay = angley * pi / 180
        sx = sin ax
        sy = sin ay
        cx = cos ax
        cy = cos ay
        px = x center + radius*cy*sx
        py = y center + radius*sy
        pz = z center + radius*cx*cy
    in
        vec3 px py pz


-- | Compute the given vectors' cross product.
cross :: Vector3 -> Vector3 -> Vector3
(Vector3 ax ay az) `cross` (Vector3 bx by bz) =
    Vector3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
