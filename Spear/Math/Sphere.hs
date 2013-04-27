module Spear.Math.Sphere
where

import Spear.Math.Vector

import Data.List (foldl')

-- | A sphere in 3D space.
data Sphere = Sphere
    { center :: {-# UNPACK #-} !Vector3
    , radius :: {-# UNPACK #-} !Float
    }

-- | Create a sphere from the given points.
sphere :: [Vector3] -> Sphere
sphere [] = Sphere zero3 0
sphere (x:xs) = Sphere c r
    where
        c = pmin + (pmax-pmin)/2
        r = norm $ pmax - c
        (pmin,pmax) = foldl' update (x,x) xs
        update (pmin,pmax) p = (min p pmin, max p pmax)

-- | Return 'True' if the given sphere contains the given point, 'False' otherwise.
circlept :: Sphere -> Vector3 -> Bool
circlept (Sphere c r) p = r*r >= normSq (p - c)
