module Spear.Math.Spatial2
where


import Spear.Math.Vector2
import Spear.Math.Matrix3 as M


-- | An entity that can be moved around in 2D space.
class Spatial2 s where
    
    -- | Move the spatial.
    move :: Vector2 -> s -> s
    
    -- | Move the spatial forwards.
    moveFwd :: Float -> s -> s
    
    -- | Move the spatial backwards.
    moveBack :: Float -> s -> s
    
    -- | Make the spatial strafe left.
    strafeLeft :: Float -> s -> s
    
    -- | Make the spatial Strafe right.
    strafeRight :: Float -> s -> s
    
    -- | Rotate the spatial.
    rotate :: Float -> s -> s
    
    -- | Get the spatial position.
    pos :: s -> Vector2
    
    -- | Get the spatial's forward vector.
    fwd :: s -> Vector2
        
    -- | Get the spatial's right vector.
    right :: s -> Vector2
    
    -- | Get the spatial's transform.
    transform :: s -> Matrix3
    
    -- | Set the spatial's transform.
    setTransform :: Matrix3 -> s -> s
    
    -- | Set the spatial's position.
    setPos :: Vector2 -> s -> s
    
    -- | Make the spatial look at the given point.
    lookAt :: Vector2 -> s -> s
    lookAt pt s =
        let position = pos s
            fwd      = normalise $ pt - position
            r        = perp fwd
        in
            setTransform (M.transform r fwd position) s
    
    -- | Make the 'Spatial' orbit around the given point
    orbit :: Vector2 -- ^ Target point
          -> Float   -- ^ Angle
          -> Float   -- ^ Orbit radius
          -> s
          -> s
    
    orbit pt angle radius s =
        let a = angle * pi / 180
            px = (x pt) + radius * sin a
            py = (y pt) + radius * cos a
        in
            setPos (vec2 px py) s
