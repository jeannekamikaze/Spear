module Spear.Math.Sphere
(
    Sphere(..)
,   sphere
,   spherept
)
where


import Spear.Math.Vector3 as Vector


-- | A bounding volume.
data Sphere = Sphere
    { center :: {-# UNPACK #-} !Vector3
    , radius :: {-# UNPACK #-} !Float
    }


-- | Create a 'Sphere' from the given points.
sphere :: [Vector3] -> Sphere

sphere [] = error "Attempting to build a BoundingVolume from an empty list!"

sphere (x:xs) = Sphere c r
    where
        c = min + (max-min)/2
        r = norm $ max - c
        (min,max) = foldr update (x,x) xs
        update p (min,max) = (Vector.min p min, Vector.max p max)


-- | Return 'True' if the given 'Sphere' contains the given point, 'False' otherwise.         
spherept :: Sphere -> Vector3 -> Bool
(Sphere center radius) `spherept` p = radius*radius >= normSq (p - center)
