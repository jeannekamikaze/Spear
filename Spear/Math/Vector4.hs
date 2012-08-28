module Spear.Math.Vector4
(
    Vector4
    -- * Accessors
,   x
,   y
,   z
,   w
    -- * Construction
,   unitX
,   unitY
,   unitZ
,   fromList
,   vec4
    -- * Operations
,   Spear.Math.Vector4.min
,   Spear.Math.Vector4.max
,   dot
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
data Vector4 = Vector4
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    deriving (Eq, Show)


instance Num Vector4 where
    Vector4 ax ay az aw + Vector4 bx by bz bw = Vector4 (ax + bx) (ay + by) (az + bz) (aw + bw)
    Vector4 ax ay az aw - Vector4 bx by bz bw = Vector4 (ax - bx) (ay - by) (az - bz) (aw - bw)
    Vector4 ax ay az aw * Vector4 bx by bz bw = Vector4 (ax * bx) (ay * by) (az * bz) (aw * bw)
    abs (Vector4 ax ay az aw) = Vector4 (abs ax) (abs ay) (abs az) (abs aw)
    signum (Vector4 ax ay az aw) = Vector4 (signum ax) (signum ay) (signum az) (signum aw)
    fromInteger i = Vector4 i' i' i' i' where i' = fromInteger i
    
    
instance Fractional Vector4 where
    Vector4 ax ay az aw / Vector4 bx by bz bw = Vector4 (ax / bx) (ay / by) (az / bz) (aw / bw)
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


sizeFloat = sizeOf (undefined :: CFloat)


instance Storable Vector4 where
    sizeOf _    = 4*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        ax <- peekByteOff ptr 0
        ay <- peekByteOff ptr $ 1 * sizeFloat
        az <- peekByteOff ptr $ 2 * sizeFloat
        aw <- peekByteOff ptr $ 3 * sizeFloat
        return (Vector4 ax ay az aw)
        
    poke ptr (Vector4 ax ay az aw) = do
        pokeByteOff ptr 0 ax
        pokeByteOff ptr (1 * sizeFloat) ay
        pokeByteOff ptr (2 * sizeFloat) az
        pokeByteOff ptr (3 * sizeFloat) aw


x (Vector4 ax _  _  _ ) = ax
y (Vector4 _  ay _  _ ) = ay
z (Vector4 _  _  az _ ) = az
w (Vector4 _  _  _  aw) = aw


-- | Unit vector along the X axis.
unitX :: Vector4
unitX = Vector4 1 0 0 0


-- | Unit vector along the Y axis.
unitY :: Vector4
unitY = Vector4 0 1 0 0


-- | Unit vector along the Z axis.
unitZ :: Vector4
unitZ = Vector4 0 0 1 0


-- | Create a vector from the given list.
fromList :: [Float] -> Vector4
fromList (ax:ay:az:aw:_) = Vector4 ax ay az aw


-- | Create a 4D vector from the given values.
vec4 :: Float -> Float -> Float -> Float -> Vector4
vec4 ax ay az aw = Vector4 ax ay az aw


-- | Create a vector whose components are the minimum of each of the given vectors'.
min :: Vector4 -> Vector4 -> Vector4
min (Vector4 ax ay az aw) (Vector4 bx by bz bw) =
    Vector4 (Prelude.min ax bx) (Prelude.min ay by) (Prelude.min az bz) (Prelude.min aw bw)


-- | Create a vector whose components are the maximum of each of the given vectors'.
max :: Vector4 -> Vector4 -> Vector4
max (Vector4 ax ay az aw) (Vector4 bx by bz bw) =
    Vector4 (Prelude.max ax bx) (Prelude.max ay by) (Prelude.max az bz) (Prelude.min aw bw)


-- | Compute the given vectors' dot product.
dot :: Vector4 -> Vector4 -> Float
Vector4 ax ay az aw `dot` Vector4 bx by bz bw = ax*bx + ay*by + az*bz + aw*bw


-- | Compute the given vectors' cross product.
-- The vectors are projected to 3D space. The resulting vector is the cross product of the vectors' projections with w=0.
cross :: Vector4 -> Vector4 -> Vector4
(Vector4 ax ay az _) `cross` (Vector4 bx by bz _) =
    Vector4 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx) 0
    
    
-- | Compute the given vector's squared norm.
normSq :: Vector4 -> Float
normSq (Vector4 ax ay az aw) = ax*ax + ay*ay + az*az + aw*aw
    

-- | Compute the given vector's norm.
norm :: Vector4 -> Float
norm = sqrt . normSq


-- | Multiply the given vector with the given scalar.
scale :: Float -> Vector4 -> Vector4
scale s (Vector4 ax ay az aw) = Vector4 (s*ax) (s*ay) (s*az) (s*aw)


-- | Normalise the given vector.
normalise :: Vector4 -> Vector4
normalise v =
    let n' = norm v
        n = if n' == 0 then 1 else n'
    in
        scale (1.0 / n) v


-- | Negate the given vector.
neg :: Vector4 -> Vector4
neg (Vector4 ax ay az aw) = Vector4 (-ax) (-ay) (-az) (-aw)
