module Spear.Math.Vector.Vector4
(
    Vector4
    -- * Construction
,   unitx4
,   unity4
,   unitz4
,   vec4
    -- * Operations
,   cross'
)
where


import Spear.Math.Vector.Class

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

    min (Vector4 ax ay az aw) (Vector4 bx by bz bw) =
        Vector4 (Prelude.min ax bx) (Prelude.min ay by) (Prelude.min az bz) (Prelude.min aw bw)

    max (Vector4 ax ay az aw) (Vector4 bx by bz bw) =
        Vector4 (Prelude.max ax bx) (Prelude.max ay by) (Prelude.max az bz) (Prelude.min aw bw)


instance VectorClass Vector4 where
         fromList (ax:ay:az:aw:_) = Vector4 ax ay az aw

         x (Vector4 ax _  _  _ ) = ax
         
         y (Vector4 _  ay _  _ ) = ay
         
         z (Vector4 _  _  az _ ) = az
         
         w (Vector4 _  _  _  aw) = aw

         (Vector4 ax _ _ _) ! 0 = ax
         (Vector4 _ ay _ _) ! 1 = ay
         (Vector4 _ _ az _) ! 2 = az
         (Vector4 _ _ _ aw) ! 3 = aw
         _                  ! _ = 0
         
         Vector4 ax ay az aw `dot` Vector4 bx by bz bw = ax*bx + ay*by + az*bz + aw*bw
         
         normSq (Vector4 ax ay az aw) = ax*ax + ay*ay + az*az + aw*aw
         
         norm = sqrt . normSq
         
         scale s (Vector4 ax ay az aw) = Vector4 (s*ax) (s*ay) (s*az) (s*aw)
         
         neg (Vector4 ax ay az aw) = Vector4 (-ax) (-ay) (-az) (-aw)
         
         normalise v =
                   let n' = norm v
                       n = if n' == 0 then 1 else n'
                   in scale (1.0 / n) v


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
vec4 ax ay az aw = Vector4 ax ay az aw


-- | Compute the given vectors' cross product.
-- The vectors are projected to 3D space. The resulting vector is the cross product of the vectors' projections with w=0.
cross' :: Vector4 -> Vector4 -> Vector4
(Vector4 ax ay az _) `cross'` (Vector4 bx by bz _) =
         Vector4 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx) 0
