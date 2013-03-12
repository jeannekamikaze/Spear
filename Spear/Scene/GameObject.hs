module Spear.Scene.GameObject
(
    GameObject
,   GameStyle(..)
,   Window(..)
,   AM.AnimationSpeed
    -- * Construction
,   goNew
    -- * Accessors
,   currentAnimation
,   goAABB
,   goAABBs
,   collisioners
,   goRPGtransform
,   numCollisioners
,   renderer
,   window
    -- * Manipulation
,   goUpdate
,   setAnimation
,   setAnimationSpeed
,   setAxis
,   withCollisioners
,   setCollisioners
,   setWindow
    -- * Rendering
,   goRender
    -- * Collision
,   goCollide
)
where


import Spear.Collision as Col
import Spear.GL
import Spear.Math.AABB
import qualified Spear.Math.Camera as Cam
import qualified Spear.Math.Matrix3 as M3
import qualified Spear.Math.Matrix4 as M4
import Spear.Math.MatrixUtils
import qualified Spear.Math.Spatial2 as S2
import qualified Spear.Math.Spatial3 as S3
import Spear.Math.Utils
import Spear.Math.Vector
import qualified Spear.Render.AnimatedModel as AM
import Spear.Render.Program
import Spear.Render.StaticModel as SM

import Data.Fixed (mod')
import Data.List (foldl')


-- | Game style.
data GameStyle
    = RPG -- ^ RPG or RTS style game.
    | PLT -- ^ Platformer or space invaders style game.


data Window = Window
    { projInv :: !M4.Matrix4
    , viewInv :: !M4.Matrix4
    , vpx     :: !Float
    , vpy     :: !Float
    , width   :: !Float
    , height  :: !Float
    }


dummyWindow = Window M4.id M4.id 0 0 640 480


-- | An object in the game scene.
data GameObject = GameObject
    { gameStyle    :: !GameStyle
    , renderer     :: !(Either StaticModelRenderer AM.AnimatedModelRenderer)
    , collisioners :: ![Collisioner]
    , transform    :: !M3.Matrix3
    , axis         :: !Vector3
    , angle        :: !Float
    , window       :: !Window
    }


instance S2.Spatial2 GameObject where

    move v go = go
        { collisioners = fmap (Col.move v) $ collisioners go
        , transform  = M3.translv v * transform go
        }

    moveFwd s go =
        let m = transform go
            v = scale s $ M3.forward m
        in go
           { collisioners = fmap (Col.move v) $ collisioners go
           , transform = M3.translv v * m
           }

    moveBack s go =
        let m = transform go
            v = scale (-s) $ M3.forward m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
            , transform = M3.translv v * m
            }

    strafeLeft s go =
        let m = transform go
            v = scale (-s) $ M3.right m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
            , transform = M3.translv v * m
            }

    strafeRight s go =
        let m = transform go
            v = scale s $ M3.right m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
            , transform = M3.translv v * m
            }

    rotate a go =
        go
        { transform = transform go * M3.rot a
        , angle = (angle go + a) `mod'` 360
        }

    setRotation a go =
        go
        { transform = M3.translation (transform go) * M3.rot a
        , angle = a
        }

    pos go = M3.position . transform $ go

    fwd go = M3.forward . transform $ go

    up go = M3.up . transform $ go

    right go = M3.right . transform $ go

    transform go = Spear.Scene.GameObject.transform go

    setTransform mat go = go { transform = mat }

    setPos pos go =
        let m = transform go
        in go { transform = M3.transform (M3.right m) (M3.forward m) pos }

    lookAt p go =
        let position = S2.pos go
            fwd      = normalise $ p - position
            r        = perp fwd
            toDeg = (*(180/pi))
            viewI = viewInv . window $ go
            p1    = viewToWorld2d position viewI
            p2    = viewToWorld2d (position + fwd) viewI
            f     = normalise $ p2 - p1
        in
            go
            { transform = M3.transform r fwd position
            , angle = 180 -
                if x f > 0
                then toDeg . acos $ f `dot` unity2
                else (+180) . toDeg . acos $ f `dot` (-unity2)
            }


-- | Create a new game object.
goNew :: GameStyle
      -> Either StaticModelResource AM.AnimatedModelResource
      -> [Collisioner]
      -> M3.Matrix3 -- ^ Transform
      -> Vector3 -- ^ Axis of rotation
      -> GameObject

goNew style (Left smr) cols transf axis = GameObject
    style (Left $ SM.staticModelRenderer smr) cols transf axis 0 dummyWindow

goNew style (Right amr) cols transf axis = GameObject
    style (Right $ AM.animatedModelRenderer 1 amr) cols transf axis 0 dummyWindow


goUpdate :: Float -> GameObject -> GameObject
goUpdate dt go =
    let rend = renderer go
        rend' = case rend of
            Left _  -> rend
            Right amr -> Right $ AM.update dt amr
    in go
       { renderer = rend'
       }


-- | Get the game object's ith bounding box.
goAABB :: Int -> GameObject -> AABB
goAABB i = getAABB . flip (!!) i . collisioners


-- | Get the game object's bounding boxes.
goAABBs :: GameObject -> [AABB]
goAABBs = fmap getAABB . collisioners


-- | Get the game object's 3D transform.
goRPGtransform :: GameObject -> M4.Matrix4
goRPGtransform go =
    let viewI = viewInv . window $ go
    in rpgTransform 0 (angle go) (axis go) (S2.pos go) viewI


-- | Get the game object's current animation.
currentAnimation :: Enum a => GameObject -> a
currentAnimation go = case renderer go of
    Left _ -> toEnum 0
    Right amr -> AM.currentAnimation amr


-- | Return the game object's number of collisioners.
numCollisioners :: GameObject -> Int
numCollisioners = length . collisioners


-- | Set the game object's current animation.
setAnimation :: Enum a => a -> GameObject -> GameObject
setAnimation a go = case renderer go of
    Left _ -> go
    Right amr -> go { renderer = Right $ AM.setAnimation a amr }


-- | Set the game object's animation speed.
setAnimationSpeed :: AM.AnimationSpeed -> GameObject -> GameObject
setAnimationSpeed s go = case renderer go of
    Left _ -> go
    Right amr -> go { renderer = Right $ AM.setAnimationSpeed s amr }


-- | Set the game object's axis of rotation.
setAxis :: Vector3 -> GameObject -> GameObject
setAxis ax go = go { axis = ax }


-- | Set the game object's collisioners.
setCollisioners :: [Collisioner] -> GameObject -> GameObject
setCollisioners cols go = go { collisioners = cols }


-- | Set the game object's window.
setWindow :: Window -> GameObject -> GameObject
setWindow wnd go = go { window = wnd }


-- | Manipulate the game object's collisioners.
withCollisioners :: GameObject -> ([Collisioner] -> [Collisioner]) -> GameObject
withCollisioners go f = go { collisioners = f $ collisioners go }


-- | Render the game object.
goRender :: StaticProgram -> AnimatedProgram -> Cam.Camera -> GameObject -> IO ()
goRender sprog aprog cam go =
    let spu = staticProgramUniforms sprog
        apu = animatedProgramUniforms aprog
        style  = gameStyle go
        axis'  = axis go
        a      = angle go
        proj   = Cam.projection cam
        view   = M4.inverseTransform $ S3.transform cam
        transf = S2.transform go
        normal = fastNormalMatrix modelview
        modelview = case style of
            RPG -> view * goRPGtransform go
            PLT -> view * pltTransform transf
    in case renderer go of
        Left smr  ->
            goRender' style a axis' sprog spu modelview proj normal
                      (SM.bind spu smr) (SM.render spu smr)
        Right amr ->
            goRender' style a axis' aprog apu modelview proj normal
                      (AM.bind apu amr) (AM.render apu amr)


type Bind = IO ()

type Render = IO ()


goRender' :: (ProgramUniforms u, Program p)
          => GameStyle
          -> Float
          -> Vector3
          -> p
          -> u
          -> M4.Matrix4 -- Modelview
          -> M4.Matrix4 -- Projection
          -> M3.Matrix3 -- Normal matrix
          -> Bind
          -> Render
          -> IO ()
goRender' style a axis prog uniforms modelview proj normal bindRenderer render =
    let
    in do
        useProgram . program $ prog
        uniform (projLoc uniforms) proj
        uniform (modelviewLoc uniforms) modelview
        uniform (normalmatLoc uniforms) normal
        bindRenderer
        render


-- | Return 'True' if the given game objects collide, 'False' otherwise.
goCollide :: GameObject -> GameObject -> Bool
goCollide go1 go2 =
    let cols1 = collisioners go1
        cols2 = collisioners go2
        c1 = cols1 !! 0
        c2 = cols2 !! 0
    in
        if length cols1 == 0 || length cols2 == 0 then False
        else c1 `collide` c2 /= NoCollision
