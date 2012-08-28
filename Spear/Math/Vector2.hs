module Spear.Math.Vector2
(
    Vector2
    -- * Accessors
,   x
,   y
    -- * Construction
,   unitx
,   unity
,   zero
,   fromList
,   vec2
    -- * Operations
,   v2min
,   v2max
,   dot
,   normSq
,   norm
,   scale
,   normalise
,   neg
,   perp
)
where

import Foreign.C.Types (CFloat)
import Foreign.Storable


-- | Represents a vector in 2D.
data Vector2 = Vector2 {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving (Eq, Show)


instance Num Vector2 where
    Vector2 ax ay + Vector2 bx by = Vector2 (ax + bx) (ay + by)
    Vector2 ax ay - Vector2 bx by = Vector2 (ax - bx) (ay - by)
    Vector2 ax ay * Vector2 bx by = Vector2 (ax * bx) (ay * by)
    abs (Vector2 ax ay) = Vector2 (abs ax) (abs ay)
    signum (Vector2 ax ay) = Vector2 (signum ax) (signum ay)
    fromInteger i = Vector2 i' i' where i' = fromInteger i
    
    
instance Fractional Vector2 where
    Vector2 ax ay / Vector2 bx by = Vector2 (ax / bx) (ay / by)
    fromRational r = Vector2 r' r' where r' = fromRational r
    
    
instance Ord Vector2 where
    Vector2 ax ay <= Vector2 bx by =  (ax <= bx) || (ax == bx && ay <= by)
    Vector2 ax ay >= Vector2 bx by =  (ax >= bx) || (ax == bx && ay >= by)
    Vector2 ax ay < Vector2 bx by  =  (ax < bx)  || (ax == bx && ay < by)
    Vector2 ax ay > Vector2 bx by  =  (ax > bx)  || (ax == bx && ay > by)


sizeFloat = sizeOf (undefined :: CFloat)


instance Storable Vector2 where
    sizeOf _    = 2*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        ax <- peekByteOff ptr 0
        ay <- peekByteOff ptr $ sizeFloat
        return (Vector2 ax ay)
    
    poke ptr (Vector2 ax ay) = do
        pokeByteOff ptr 0 ax
        pokeByteOff ptr sizeFloat ay


-- | Get the vector's x coordinate.    
x (Vector2 ax _) = ax


-- | Get the vector's y coordinate.
y (Vector2 _ ay) = ay


-- | Unit vector along the X axis.
unitx :: Vector2
unitx = Vector2 1 0


-- | Unit vector along the Y axis.
unity :: Vector2
unity = Vector2 0 1


-- | Zero vector.
zero :: Vector2
zero = Vector2 0 0


-- | Create a vector from the given list.
fromList :: [Float] -> Vector2
fromList (ax:ay:_) = Vector2 ax ay


-- | Create a vector from the given values.
vec2 :: Float -> Float -> Vector2
vec2 ax ay = Vector2 ax ay


-- | Create a vector with components set to the minimum of each of the given vectors'.
v2min :: Vector2 -> Vector2 -> Vector2
v2min (Vector2 ax ay) (Vector2 bx by) = Vector2 (Prelude.min ax bx) (Prelude.min ay by)


-- | Create a vector with components set to the maximum of each of the given vectors'.
v2max :: Vector2 -> Vector2 -> Vector2
v2max (Vector2 ax ay) (Vector2 bx by) = Vector2 (Prelude.max ax bx) (Prelude.max ay by)


-- | Compute the given vectors' dot product.
dot :: Vector2 -> Vector2 -> Float
Vector2 ax ay `dot` Vector2 bx by = ax*bx + ay*by


-- | Compute the given vector's squared norm.
normSq :: Vector2 -> Float
normSq (Vector2 ax ay) = ax*ax + ay*ay
    

-- | Compute the given vector's norm.
norm :: Vector2 -> Float
norm = sqrt . normSq


-- | Multiply the given vector with the given scalar.
scale :: Float -> Vector2 -> Vector2
scale s (Vector2 ax ay) = Vector2 (s*ax) (s*ay)


-- | Normalise the given vector.
normalise :: Vector2 -> Vector2
normalise v =
    let n' = norm v
        n = if n' == 0 then 1 else n'
    in
        scale (1.0 / n) v


-- | Negate the given vector.
neg :: Vector2 -> Vector2
neg (Vector2 ax ay) = Vector2 (-ax) (-ay)


-- | Compute a vector perpendicular to the given one, satisfying:
--
-- perp (Vector2 0 1) = Vector2 1 0
-- 
-- perp (Vector2 1 0) = Vector2 0 (-1)
perp :: Vector2 -> Vector2
perp (Vector2 x y) = Vector2 y (-x)
