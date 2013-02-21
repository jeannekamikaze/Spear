module Spear.Math.Quad
(
    Quad(..)
,   quadpt
)
where


import Spear.Math.Segment
import Spear.Math.Utils
import Spear.Math.Vector


data Quad = Quad
    { tl :: {-# UNPACK #-} !Vector2 -- ^ Top left
    , tr :: {-# UNPACK #-} !Vector2 -- ^ Top right
    , br :: {-# UNPACK #-} !Vector2 -- ^ Bottom right
    , bl :: {-# UNPACK #-} !Vector2 -- ^ Bottom left
    }


-- | Return 'True' if the given point is inside the given quad, 'False' otherwise.
quadpt :: Quad -> Vector2 -> Bool
quadpt (Quad tl tr br bl) p =
    let
        s1 = seglr (Segment tl tr) p
        s2 = seglr (Segment tr br) p
        s3 = seglr (Segment br bl) p
        s4 = seglr (Segment bl tl) p
    in
        R == s1 && s1 == s2 && s2 == s3 && s3 == s4
