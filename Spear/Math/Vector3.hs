module Spear.Math.Vector3
(
    Vector3
    -- * Accessors
,   x
,   y
,   z
    -- * Construction
,   unitx
,   unity
,   unitz
,   zero
,   fromList
,   vec3
,   orbit
    -- * Operations
,   Spear.Math.Vector3.min
,   Spear.Math.Vector3.max
,   dot
,   cross
,   normSq
,   norm
,   scale
,   normalise
,   neg
)
where

import Foreign.C.Types (CFloat)
import Foreign.Storable


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


x (Vector3 ax _  _ ) = ax
y (Vector3 _  ay _ ) = ay
z (Vector3 _  _  az) = az


-- | Unit vector along the X axis.
unitx :: Vector3
unitx = Vector3 1 0 0


-- | Unit vector along the Y axis.
unity :: Vector3
unity = Vector3 0 1 0


-- | Unit vector along the Z axis.
unitz :: Vector3
unitz = Vector3 0 0 1


-- | Zero vector.
zero :: Vector3
zero = Vector3 0 0 0


-- | Create a vector from the given list.
fromList :: [Float] -> Vector3
fromList (ax:ay:az:_) = Vector3 ax ay az


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


-- | Create a vector with components set to the minimum of each of the given vectors'.
min :: Vector3 -> Vector3 -> Vector3
min (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (Prelude.min ax bx) (Prelude.min ay by) (Prelude.min az bz)


-- | Create a vector with components set to the maximum of each of the given vectors'.
max :: Vector3 -> Vector3 -> Vector3
max (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (Prelude.max ax bx) (Prelude.max ay by) (Prelude.max az bz)


-- | Compute the given vectors' dot product.
dot :: Vector3 -> Vector3 -> Float
Vector3 ax ay az `dot` Vector3 bx by bz = ax*bx + ay*by + az*bz


-- | Compute the given vectors' cross product.
cross :: Vector3 -> Vector3 -> Vector3
(Vector3 ax ay az) `cross` (Vector3 bx by bz) =
    Vector3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
    
    
-- | Compute the given vector's squared norm.
normSq :: Vector3 -> Float
normSq (Vector3 ax ay az) = ax*ax + ay*ay + az*az
    

-- | Compute the given vector's norm.
norm :: Vector3 -> Float
norm = sqrt . normSq


-- | Multiply the given vector with the given scalar.
scale :: Float -> Vector3 -> Vector3
scale s (Vector3 ax ay az) = Vector3 (s*ax) (s*ay) (s*az)


-- | Normalise the given vector.
normalise :: Vector3 -> Vector3
normalise v =
    let n' = norm v
        n = if n' == 0 then 1 else n'
    in
        scale (1.0 / n) v


-- | Negate the given vector.
neg :: Vector3 -> Vector3
neg (Vector3 ax ay az) = Vector3 (-ax) (-ay) (-az)
