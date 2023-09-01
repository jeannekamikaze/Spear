{-# LANGUAGE FlexibleContexts #-}

module Spear.Math.Vector.Vector where

import           Spear.Math.Algebra


class
  ( Addition v v
  , Subtraction v v
  , Product v v v
  , Product v Float v  -- Scalar product.
  , Product Float v v) -- Scalar product.
  => Vector v where
      -- | Create a vector from the given list.
      fromList :: [Float] -> v

      -- | Get the vector's x coordinate.
      x :: v -> Float
      x _ = 0

      -- | Get the vector's y coordinate.
      y :: v -> Float
      y _ = 0

      -- | Get the vector's z coordinate.
      z :: v -> Float
      z _ = 0

      -- | Get the vector's w coordinate.
      w :: v -> Float
      w _ = 0

      -- | Get the vector's ith coordinate.
      (!) :: v -> Int -> Float

      -- | Compute the given vectors' dot product.
      dot :: v -> v -> Float

      -- | Compute the given vector's squared norm.
      normSq :: v -> Float

      -- | Compute the given vector's norm.
      norm :: v -> Float

      -- | Negate the given vector.
      neg :: v -> v

      -- | Normalise the given vector.
      normalise :: v -> v
