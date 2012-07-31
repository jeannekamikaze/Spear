module Spear.Collision.Types
where

-- | Encodes several collision situations.
data CollisionType = NoCollision | Collision | FullyContains | FullyContainedBy | Equal
    deriving (Eq, Ord, Show)
