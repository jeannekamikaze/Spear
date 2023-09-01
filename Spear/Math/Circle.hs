{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Spear.Math.Circle
where

import           Spear.Math.Algebra
import           Spear.Math.Vector
import           Spear.Prelude

import           Data.List           (foldl')
import           Spear.Math.Spatial
import           Spear.Math.Spatial2


-- | A circle in 2D space.
data Circle = Circle
    { center :: {-# UNPACK #-} !Vector2
    , radius :: {-# UNPACK #-} !Float
    }


instance Positional Circle Vector2 where
    setPosition p circle = circle { center = p }
    position = center
    translate v circle = circle { center = center circle + v}


-- | Create a circle from the given points.
circle :: [Vector2] -> Circle
circle [] = Circle zero2 0
circle (x:xs) = Circle c r
    where
        c = pmin + (pmax-pmin) / (2::Float)
        r = norm $ pmax - c
        (pmin,pmax) = foldl' update (x,x) xs
        update (pmin,pmax) p = (min p pmin, max p pmax)

-- | Return 'True' if the given circle contains the given point, 'False' otherwise.
circlept :: Circle -> Vector2 -> Bool
circlept (Circle c r) p = r*r >= normSq (p - c)
