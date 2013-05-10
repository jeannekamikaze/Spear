module Spear.Math.Circle
where

import Spear.Math.Vector

import Data.List (foldl')

-- | A circle in 2D space.
data Circle = Circle
    { center :: {-# UNPACK #-} !Vector2
    , radius :: {-# UNPACK #-} !Float
    }

-- | Create a circle from the given points.
circle :: [Vector2] -> Circle
circle [] = Circle zero2 0
circle (x:xs) = Circle c r
    where
        c = pmin + (pmax-pmin)/2
        r = norm $ pmax - c
        (pmin,pmax) = foldl' update (x,x) xs
        update (pmin,pmax) p = (min p pmin, max p pmax)

-- | Return 'True' if the given circle contains the given point, 'False' otherwise.
circlept :: Circle -> Vector2 -> Bool
circlept (Circle c r) p = r*r >= normSq (p - c)
