module Spear.Math.Vector.Class
where

class (Fractional a, Ord a) => VectorClass a where
      -- | Create a vector from the given list.
      fromList :: [Float] -> a
      
      -- | Return the vector's x coordinate.
      x :: a -> Float
      x _ = 0

      -- | Return the vector's y coordinate.
      y :: a -> Float
      y _ = 0

      -- | Return the vector's z coordinate.
      z :: a -> Float
      z _ = 0

      -- | Return the vector's w coordinate.
      w :: a -> Float
      w _ = 0

      -- | Return the vector's ith coordinate.
      (!) :: a -> Int -> Float
      
      -- | Compute the given vectors' dot product.
      dot :: a -> a -> Float
      
      -- | Compute the given vector's squared norm.
      normSq :: a -> Float
      
      -- | Compute the given vector's norm.
      norm :: a -> Float
      
      -- | Multiply the given vector with the given scalar.
      scale :: Float -> a -> a
      
      -- | Negate the given vector.
      neg :: a -> a
      
      -- | Normalise the given vector.
      normalise :: a -> a