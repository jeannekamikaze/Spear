{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Spear.Math.Algebra
where

import           Foreign.C.Types
import           Prelude         hiding ((*), (+), (-), (/))
import qualified Prelude         as P


-- | General addition.
class Addition a b where
  infixl 6 +
  (+) :: a -> b -> a

-- | General subtraction.
class Subtraction a b where
  infixl 6 -
  (-) :: a -> b -> a

-- | General multiplication.
class Product a b c | a b -> c where
  infixl 7 *
  (*) :: a -> b -> c

-- | General division.
class Quotient a b where
  infixl 7 /
  (/) :: a -> b -> a

-- -- Commutative addition.
-- class CommutativeAddition a b

-- -- Commutative product.
-- class CommutativeProduct a b


-- Convenient definitions so that we can again use operators on scalars simply.
instance Addition Int Int where (+) = (P.+)
instance Addition Float Float where (+) = (P.+)
instance Addition Double Double where (+) = (P.+)
instance Addition CUInt CUInt where (+) = (P.+)

instance Subtraction Int Int where (-) = (P.-)
instance Subtraction Float Float where (-) = (P.-)
instance Subtraction Double Double where (-) = (P.-)

instance Product Int Int Int where (*) = (P.*)
instance Product Float Float Float where (*) = (P.*)
instance Product Double Double Double where (*) = (P.*)
instance Product CUInt CUInt CUInt where (*) = (P.*)

instance Quotient Int Int where (/) = P.div
instance Quotient Float Float where (/) = (P./)
instance Quotient Double Double where (/) = (P./)


-- These definitions help in the implementations of Num. Num is needed if we
-- want syntactic negation for a type.
add :: Addition a a => a -> a -> a
add a b = a + b

sub :: Subtraction a a => a -> a -> a
sub a b = a - b

mul :: Product a a a => a -> a -> a
mul a b = a * b

div :: Quotient a a => a -> a -> a
div a b = a / b


{- instance Num a => Addition a a where
  (+) = (P.+)

instance Num a => Subtraction a a where
  (-) = (P.+)

instance Num a => Product a a where
  type Prod a a = a

  (*) = (P.*)

instance Fractional a => Quotient a a where
  (/) = (P./) -}


-- instance Quotient Int Int where (/) = div

-- instance (Addition a b c, CommutativeAddition a b) => Addition b a c where
--   b + a = a + b

-- instance (Product a b c, CommutativeProduct a b) => Product b a c where
--   b * a = a * b

-- instance Num a => CommutativeAddition a a
-- instance Num a => CommutativeProduct  a a


lerp a b t = a + t * (b - a)
