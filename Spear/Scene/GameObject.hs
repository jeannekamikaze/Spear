module Spear.Scene.GameObject
(
    GameObject
,   GameStyle(..)
,   AM.AnimationSpeed
    -- * Construction
,   goNew
    -- * Manipulation
,   goUpdate
,   currentAnimation
,   setAnimation
,   setAnimationSpeed
,   goAABB
    -- * Rendering
,   goRender
    -- * Collision
,   goCollide
)
where


import Spear.Collision.Collision
import Spear.Collision.Collisioner as Col
import Spear.GLSL.Management
import Spear.GLSL.Uniform
import Spear.Math.AABB
import qualified Spear.Math.Camera as Cam
import qualified Spear.Math.Matrix3 as M3
import qualified Spear.Math.Matrix4 as M4
import Spear.Math.MatrixUtils
import qualified Spear.Math.Spatial2 as S2
import Spear.Math.Vector2 as V2
import Spear.Math.Vector3 as V3
import qualified Spear.Render.AnimatedModel as AM
import Spear.Render.Program
import Spear.Render.StaticModel as SM

import Data.List (foldl')


-- | Game style.
data GameStyle
    = RPG -- ^ RPG or RTS style game.
    | PLT -- ^ Platformer or space invaders style game.


-- | An object in the game scene.
data GameObject = GameObject
    { gameStyle   :: GameStyle
    , renderer    :: !(Either StaticModelRenderer AM.AnimatedModelRenderer)
    , collisioner :: !Collisioner
    , transform   :: M3.Matrix3
    }


instance S2.Spatial2 GameObject where
    
    move v go = go
        { collisioner = Col.move v $ collisioner go
        , transform  = M3.translv v * transform go
        }
    
    moveFwd s go =
        let m = transform go
            v = V2.scale s $ M3.forward m
        in go
            { collisioner = Col.move v $ collisioner go
            , transform = M3.translv v * m
            }
    
    moveBack s go =
        let m = transform go
            v = V2.scale (-s) $ M3.forward m
        in go
            { collisioner = Col.move v $ collisioner go
            , transform = M3.translv v * m
            }
    
    strafeLeft s go =
        let m = transform go
            v = V2.scale (-s) $ M3.right m
        in go
            { collisioner = Col.move v $ collisioner go
            , transform = M3.translv v * m
            }
    
    strafeRight s go =
        let m = transform go
            v = V2.scale s $ M3.right m
        in go
            { collisioner = Col.move v $ collisioner go
            , transform = M3.translv v * m
            }
    
    rotate angle go = go { transform = transform go * M3.rot angle }
    
    pos go = M3.position . transform $ go
    
    fwd go = M3.forward . transform $ go
    
    up go = M3.up . transform $ go
    
    right go = M3.right . transform $ go
    
    transform go = Spear.Scene.GameObject.transform go
    
    setTransform mat go = go { transform = mat }
    
    setPos pos go =
        let m = transform go
        in go { transform = M3.transform (M3.right m) (M3.forward m) pos }


-- | Create a new game object.
goNew :: GameStyle
      -> Either StaticModelResource AM.AnimatedModelResource
      -> Collisioner
      -> M3.Matrix3
      -> GameObject

goNew style (Left smr) col transf =
    GameObject style (Left $ SM.staticModelRenderer smr) col transf

goNew style (Right amr) col transf =
    GameObject style (Right $ AM.animatedModelRenderer 1 amr) col transf


goUpdate :: Float -> GameObject -> GameObject
goUpdate dt go =
    let rend = renderer go
        rend' = case rend of
            Left _  -> rend
            Right amr -> Right $ AM.update dt amr
    in go
       { renderer = rend'
       }


-- | Get the game object's current animation.
currentAnimation :: Enum a => GameObject -> a
currentAnimation go = case renderer go of
    Left _ -> toEnum 0
    Right amr -> AM.currentAnimation amr 


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


-- | Get the game object's bounding box.
goAABB :: GameObject -> AABB
goAABB go =
    case collisioner go of
        (AABBCol box) -> box
        (CircleCol circle) -> aabbFromCircle circle


-- | Render the game object.
goRender :: StaticProgram -> AnimatedProgram -> Cam.Camera -> GameObject -> IO ()
goRender sprog aprog cam go =
    let spu = staticProgramUniforms sprog
        apu = animatedProgramUniforms aprog
        mat = S2.transform go
        style = gameStyle go
    in case renderer go of
        Left smr  -> goRender' style sprog spu mat cam (SM.bind spu smr) (SM.render spu smr)
        Right amr -> goRender' style aprog apu mat cam (AM.bind apu amr) (AM.render apu amr)


type Bind = IO ()

type Render = IO ()


goRender' :: (ProgramUniforms u, Program p)
          => GameStyle
          -> p
          -> u
          -> M3.Matrix3
          -> Cam.Camera
          -> Bind
          -> Render
          -> IO ()
goRender' style prog uniforms model cam bindRenderer render =
    let view  = M4.inverseTransform $ Cam.transform cam
        modelview = case style of
            RPG -> view * rpgTransform 0 model
            PLT -> view * pltTransform model
        normalmat = fastNormalMatrix modelview
    in do
        useProgram . program $ prog
        uniformMat4 (projLoc uniforms) $ Cam.projection cam
        uniformMat4 (modelviewLoc uniforms) modelview
        uniformMat3 (normalmatLoc uniforms) normalmat
        bindRenderer
        render


-- | Collide the game object with the given list of game objects.
goCollide :: [GameObject] -> GameObject -> [GameObject]
goCollide gos go = foldl' collide' [] gos
    where
        collide' gos target = target:gos
