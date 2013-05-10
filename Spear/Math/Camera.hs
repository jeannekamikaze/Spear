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

import qualified Spear.Math.Matrix4 as M
import Spear.Math.Spatial3
import Spear.Math.Vector

data Camera = Camera
    { projection :: M.Matrix4 -- ^ Get the camera's projection.
    , spatial    :: Obj3
    }

instance Spatial3 Camera where
    getObj3 = spatial
    setObj3 cam o = cam { spatial = o }

type Fovy = Float
type Aspect = Float
type Near = Float
type Far = Float
type Left = Float
type Right = Float
type Bottom = Float
type Top = Float

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
    , spatial    = fromVectors right up fwd pos
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
    , spatial    = fromVectors right up fwd pos
    }
