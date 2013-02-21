module Spear.Math.Vector.Vector2
(
    Vector2
    -- * Construction
,   unitx2
,   unity2
,   zero2
,   vec2
    -- * Operations
,   perp
)
where


import Spear.Math.Vector.Class


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
    max (Vector2 ax ay) (Vector2 bx by) = Vector2 (Prelude.max ax bx) (Prelude.max ay by)
    min (Vector2 ax ay) (Vector2 bx by) = Vector2 (Prelude.min ax bx) (Prelude.min ay by)


instance VectorClass Vector2 where
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

         {-# INLINABLE scale #-}
         scale s (Vector2 ax ay) = Vector2 (s*ax) (s*ay)

         {-# INLINABLE neg #-}
         neg (Vector2 ax ay) = Vector2 (-ax) (-ay)

         {-# INLINABLE normalise #-}
         normalise v =
                   let n' = norm v
                       n = if n' == 0 then 1 else n'
                   in scale (1.0 / n) v


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



-- | Unit vector along the X axis.
unitx2 = Vector2 1 0


-- | Unit vector along the Y axis.
unity2 = Vector2 0 1


-- | Zero vector.
zero2 = Vector2 0 0


-- | Create a vector from the given values.
vec2 :: Float -> Float -> Vector2
vec2 ax ay = Vector2 ax ay


-- | Compute a vector perpendicular to the given one, satisfying:
-- 
-- perp (Vector2 0 1) = Vector2 1 0
-- 
-- perp (Vector2 1 0) = Vector2 0 (-1)
perp :: Vector2 -> Vector2
perp (Vector2 x y) = Vector2 y (-x)
