module Spear.Math.Camera
where


import qualified Spear.Math.Matrix4 as M
import qualified Spear.Math.Spatial3 as S
import Spear.Math.Vector


data Camera = Camera
    { projection :: M.Matrix4
    , transform  :: M.Matrix4
    }


-- | Build a perspective camera.
perspective :: Float -- ^ Fovy - Vertical field of view angle in degrees.
            -> Float -- ^ Aspect ratio.
            -> Float -- ^ Near clip.
            -> Float -- ^ Far clip.
            -> Vector3 -- ^ Right vector.
            -> Vector3 -- ^ Up vector.
            -> Vector3 -- ^ Forward vector.
            -> Vector3 -- ^ Position vector.
            -> Camera

perspective fovy r n f right up fwd pos =
    Camera
    { projection = M.perspective fovy r n f
    , transform  = M.transform right up (neg fwd) pos
    }


-- | Build an orthogonal camera.
ortho :: Float   -- ^ Left.
      -> Float   -- ^ Right.
      -> Float   -- ^ Bottom.
      -> Float   -- ^ Top.
      -> Float   -- ^ Near clip.
      -> Float   -- ^ Far clip.
      -> Vector3 -- ^ Right vector.
      -> Vector3 -- ^ Up vector.
      -> Vector3 -- ^ Forward vector.
      -> Vector3 -- ^ Position vector.
      -> Camera

ortho l r b t n f right up fwd pos =
    Camera
    { projection = M.ortho l r b t n f
    , transform  = M.transform right up (neg fwd) pos
    }


instance S.Spatial3 Camera where
    move        v cam = cam { transform = M.translv v * transform cam }
    moveFwd     f cam = cam { transform = M.translv (scale f $ S.fwd cam) * transform cam }
    moveBack    f cam = cam { transform = M.translv (scale (-f) $ S.fwd cam) * transform cam }
    strafeLeft  f cam = cam { transform = M.translv (scale (-f) $ S.right cam) * transform cam }
    strafeRight f cam = cam { transform = M.translv (scale f $ S.right cam) * transform cam }
    pitch       a cam = cam { transform = transform cam * M.axisAngle (S.right cam) a }
    yaw         a cam = cam { transform = transform cam * M.axisAngle (S.up cam)    a }
    roll        a cam = cam { transform = transform cam * M.axisAngle (S.fwd cam)   a }
    pos   = M.position . transform
    fwd   = M.forward  . transform
    up    = M.up       . transform
    right = M.right    . transform
    transform (Camera _ t) = t
    setTransform t (Camera proj _) = Camera proj t
    setPos pos (Camera proj t) = Camera proj $
        M.transform (M.right t) (M.up t) (M.forward t) pos

