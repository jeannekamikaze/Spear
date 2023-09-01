{-# LANGUAGE MultiParamTypeClasses #-}

module Spear.Math.Camera
(
    Camera
,   Fovy
,   Aspect
,   Near
,   Far
,   Left
,   Right
,   Bottom
,   Top
,   projection
,   perspective
,   ortho
)
where

import qualified Spear.Math.Matrix4  as M
import           Spear.Math.Spatial
import           Spear.Math.Spatial3
import           Spear.Math.Vector


data Camera = Camera
    { projection :: M.Matrix4 -- ^ Get the camera's projection.
    , basis      :: Transform3
    }


instance Has3dTransform Camera where
    set3dTransform transform camera = camera { basis = transform }
    transform3 = basis


instance Positional Camera Vector3 where
    setPosition p = with3dTransform (setPosition p)
    position = position . basis
    translate v = with3dTransform (translate v)


instance Rotational Camera Vector3 Rotation3 where
    setRotation rotation = with3dTransform (setRotation rotation)
    rotation = rotation . basis
    rotate rot = with3dTransform (rotate rot)
    right = right . basis
    up = up . basis
    forward = forward . basis
    setForward forward = with3dTransform (setForward forward)


instance Spatial Camera Vector3 Rotation3 Transform3 where
    setTransform transform camera = camera { basis = transform }
    transform = basis


type Fovy   = Float
type Aspect = Float
type Near   = Float
type Far    = Float
type Left   = Float
type Right  = Float
type Bottom = Float
type Top    = Float

-- | Build a perspective camera.
perspective :: Fovy      -- ^ Fovy - Vertical field of view angle in degrees.
            -> Aspect    -- ^ Aspect ratio.
            -> Near      -- ^ Near clip.
            -> Far       -- ^ Far clip.
            -> Right3    -- ^ Right vector.
            -> Up3       -- ^ Up vector.
            -> Forward3  -- ^ Forward vector.
            -> Position3 -- ^ Position vector.
            -> Camera
perspective fovy r n f right up fwd pos =
    Camera
    { projection = M.perspective fovy r n f
    , basis      = newTransform3 right up fwd pos
    }

-- | Build an orthogonal camera.
ortho :: Left      -- ^ Left.
      -> Right     -- ^ Right.
      -> Bottom    -- ^ Bottom.
      -> Top       -- ^ Top.
      -> Near      -- ^ Near clip.
      -> Far       -- ^ Far clip.
      -> Right3    -- ^ Right vector.
      -> Up3       -- ^ Up vector.
      -> Forward3  -- ^ Forward vector.
      -> Position3 -- ^ Position vector.
      -> Camera
ortho l r b t n f right up fwd pos =
    Camera
    { projection = M.ortho l r b t n f
    , basis      = newTransform3 right up fwd pos
    }
