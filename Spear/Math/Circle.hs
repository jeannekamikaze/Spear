module Spear.Math.Circle
(
    Circle(..)
,   circle
,   circlept
)
where


import Spear.Math.Vector2


-- | A bounding volume.
data Circle = Circle
    { center :: {-# UNPACK #-} !Vector2
    , radius :: {-# UNPACK #-} !Float
    }


-- | Create a 'Sphere' from the given points.
circle :: [Vector2] -> Circle
circle [] = error "Attempting to build a Circle from an empty list!"
circle (x:xs) = Circle c r
    where
        c = pmin + (pmax-pmin)/2
        r = norm $ pmax - c
        (pmin,pmax) = foldr update (x,x) xs
        update p (pmin,pmax) = (min p pmin, max p pmax)


-- | Return 'True' if the given 'Sphere' contains the given point, 'False' otherwise.         
circlept :: Circle -> Vector2 -> Bool
circlept (Circle c r) p = r*r >= normSq (p - c)
