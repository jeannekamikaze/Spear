module Spear.Updatable
where


-- | A type class for types that can update themselves given a time delta.
class Updatable a where
    
    -- | Updates the given 'Updatable'.
    update :: Float -> a -> a
