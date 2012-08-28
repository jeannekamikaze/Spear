module Spear.Collision.Types
where

-- | Encodes several collision situations.
data CollisionType = NoCollision | Collision | FullyContains | FullyContainedBy
    deriving (Eq, Show)
