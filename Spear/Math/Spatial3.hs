module Spear.Math.Spatial3
where


import Spear.Math.Vector3
import Spear.Math.Matrix4 as M


class Spatial3 s where
    -- | Move the 'Spatial'.
    move :: Vector3 -> s -> s
    
    -- | Move the 'Spatial' forwards.
    moveFwd :: Float -> s -> s
    
    -- | Move the 'Spatial' backwards.
    moveBack :: Float -> s -> s
    
    -- | Make the 'Spatial' strafe left.
    strafeLeft :: Float -> s -> s
    
    -- | Make the 'Spatial' Strafe right.
    strafeRight :: Float -> s -> s
    
    -- | Rotate the 'Spatial' about its local X axis.
    pitch :: Float -> s -> s
    
    -- | Rotate the 'Spatial' about its local Y axis.
    yaw :: Float -> s -> s
    
    -- | Rotate the 'Spatial' about its local Z axis.
    roll :: Float -> s -> s
    
    -- | Get the 'Spatial''s position.
    pos :: s -> Vector3
    
    -- | Get the 'Spatial''s forward vector.
    fwd :: s -> Vector3
    
    -- | Get the 'Spatial''s up vector.
    up :: s -> Vector3
    
    -- | Get the 'Spatial''s right vector.
    right :: s -> Vector3
    
    -- | Get the 'Spatial''s transform.
    transform :: s -> Matrix4
    
    -- | Set the 'Spatial''s transform.
    setTransform :: Matrix4 -> s -> s
    
    -- | Set the 'Spatial''s position.
    setPos :: Vector3 -> s -> s
    
    -- | Make the 'Spatial' look at the given point.
    lookAt :: Vector3 -> s -> s
    lookAt pt s =
        let position = pos s
            fwd      = normalise $ pt - position
            r        = fwd `cross` unity
            u        = r `cross` fwd
        in
            setTransform (M.transform r u (-fwd) position) s
    
    -- | Make the 'Spatial' orbit around the given point
    orbit :: Vector3 -- ^ Target point
          -> Float   -- ^ Horizontal angle
          -> Float   -- ^ Vertical angle
          -> Float   -- ^ Orbit radius.
          -> s
          -> s
    
    orbit pt anglex angley radius s =
        let ax = anglex * pi / 180
            ay = angley * pi / 180
            sx = sin ax
            sy = sin ay
            cx = cos ax
            cy = cos ay
            px = (x pt) + radius*cy*sx
            py = (y pt) + radius*sy
            pz = (z pt) + radius*cx*cy
        in
            setPos (vec3 px py pz) s
