module Spear.Math.Segment
(
    Segment(..)
,   seglr
)
where


import Spear.Math.Utils
import Spear.Math.Vector2


-- | A line segment in 2D space.
data Segment = Segment {-# UNPACK #-} !Vector2 {-# UNPACK #-} !Vector2


-- | Classify the given point's position with respect to the given segment.
seglr :: Segment -> Vector2 -> Side
seglr (Segment p0 p1) p
    | orientation2d p0 p1 p < 0 = R
    | otherwise = L
