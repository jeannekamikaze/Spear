module Spear.Math.Frustum
where

import Spear.Math.Plane

data Frustum = Frustum
     { n :: {-# UNPACK #-} !Plane
     , f :: {-# UNPACK #-} !Plane
     , l :: {-# UNPACK #-} !Plane
     , r :: {-# UNPACK #-} !Plane
     , t :: {-# UNPACK #-} !Plane
     , b :: {-# UNPACK #-} !Plane
     } deriving Show

-- | Construct a frustum.
frustum
    :: Plane -- ^ Near
    -> Plane -- ^ Far
    -> Plane -- ^ Left
    -> Plane -- ^ Right
    -> Plane -- ^ Top
    -> Plane -- ^ Bottom
    -> Frustum
frustum = Frustum

-- | Construct a frustum.
fromList :: [Plane] -> Frustum
fromList (n:f:l:r:t:b:_) = Frustum n f l r t b
