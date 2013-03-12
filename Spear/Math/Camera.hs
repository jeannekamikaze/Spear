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
)
where


import qualified Spear.Math.Matrix4 as M
import Spear.Math.Spatial3
import Spear.Math.Vector


data Camera = Camera
    { projection :: M.Matrix4 -- ^ Get the camera's projection.
    , spatial    :: Obj3
    }

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


instance Spatial3 Camera where
    move         v cam = cam { spatial = move v        $ spatial cam }
    moveFwd      s cam = cam { spatial = moveFwd s     $ spatial cam }
    moveBack     s cam = cam { spatial = moveBack s    $ spatial cam }
    strafeLeft   s cam = cam { spatial = strafeLeft s  $ spatial cam }
    strafeRight  s cam = cam { spatial = strafeRight s $ spatial cam }
    pitch        a cam = cam { spatial = pitch a       $ spatial cam }
    yaw          a cam = cam { spatial = yaw a         $ spatial cam }
    roll         a cam = cam { spatial = roll a        $ spatial cam }
    pos            cam = pos   $ spatial cam
    fwd            cam = fwd   $ spatial cam
    up             cam = up    $ spatial cam
    right          cam = right $ spatial cam
    transform      cam = transform $ spatial cam
    setTransform m cam = cam { spatial = setTransform m $ spatial cam }
    setPos       p cam = cam { spatial = setPos p $ spatial cam }
