module Spear.Math.Spatial3
(
    Spatial3(..)
,   Obj3
,   fromVectors
,   fromTransform
)
where

import Spear.Math.Vector
import Spear.Math.Matrix4 as M hiding (scale)

class Spatial3 s where
    -- | Move the spatial.
    move :: Vector3 -> s -> s

    -- | Move the spatial forwards.
    moveFwd :: Float -> s -> s

    -- | Move the spatial backwards.
    moveBack :: Float -> s -> s

    -- | Make the spatial strafe left.
    strafeLeft :: Float -> s -> s

    -- | Make the spatial Strafe right.
    strafeRight :: Float -> s -> s

    -- | Rotate the spatial about its local X axis.
    pitch :: Float -> s -> s

    -- | Rotate the spatial about its local Y axis.
    yaw :: Float -> s -> s

    -- | Rotate the spatial about its local Z axis.
    roll :: Float -> s -> s

    -- | Get the spatial's position.
    pos :: s -> Vector3

    -- | Get the spatial's forward vector.
    fwd :: s -> Vector3

    -- | Get the spatial's up vector.
    up :: s -> Vector3

    -- | Get the spatial's right vector.
    right :: s -> Vector3

    -- | Get the spatial's transform.
    transform :: s -> Matrix4

    -- | Set the spatial's transform.
    setTransform :: Matrix4 -> s -> s

    -- | Set the spatial's position.
    setPos :: Vector3 -> s -> s

    -- | Make the spatial look at the given point.
    lookAt :: Vector3 -> s -> s
    lookAt pt s =
        let position = pos s
            fwd      = normalise $ pt - position
            r        = fwd `cross` unity3
            u        = r `cross` fwd
        in
            setTransform (M.transform r u (-fwd) position) s

    -- | Make the spatial orbit around the given point
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

-- | An object in 3D space.
data Obj3 = Obj3
     { r :: Vector3
     , u :: Vector3
     , f :: Vector3
     , p :: Vector3
     } deriving Show

instance Spatial3 Obj3 where
         move        d o = o { p = p o + d }
         moveFwd     s o = o { p = p o + scale (-s) (f o) }
         moveBack    s o = o { p = p o + scale s (f o) }
         strafeLeft  s o = o { p = p o + scale (-s) (r o) }
         strafeRight s o = o { p = p o + scale s (r o) }
         pitch       a o =
                     let a' = toRAD a
                         sa = sin a'
                         ca = cos a'
                         r' = normalise $ scale ca (r o) + scale sa (f o)
                         f' = normalise $ r' `cross` u o
                     in  o { r = r', f = f' }
         yaw         a o =
                     let a' = toRAD a
                         sa = sin a'
                         ca = cos a'
                         f' = normalise $ scale ca (f o) + scale sa (u o)
                         u' = normalise $ r o `cross` f'
                     in  o { u = u', f = f' }
         roll        a o =
                     let a' = toRAD a
                         sa = sin a'
                         ca = cos a'
                         u' = normalise $ scale ca (u o) - scale sa (r o)
                         f' = normalise $ f o `cross` u'
                     in  o { u = u', f = f' }
         pos   = p
         fwd   = f
         up    = u
         right = r
         transform o = M.transform (r o) (u o) (f o) (p o)
         setTransform t o = Obj3
                      { r = M.right t
                      , u = M.up t
                      , f = M.forward t
                      , p = M.position t
                      }
         setPos pos o = o { p = pos }

fromVectors :: Right3 -> Up3 -> Forward3 -> Position3 -> Obj3
fromVectors = Obj3

fromTransform :: Matrix4 -> Obj3
fromTransform m = Obj3 (M.right m) (M.up m) (M.forward m) (M.position m)

toRAD = (*pi) . (/180)
