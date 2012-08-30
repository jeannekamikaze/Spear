module Spear.Scene.GameObject
(
    GameObject
,   GameStyle(..)
,   AM.AnimationSpeed
    -- * Construction
,   goNew
    -- * Accessors
,   renderer
,   currentAnimation
,   numCollisioners
,   goAABB
,   goAABBs
    -- * Manipulation
,   goUpdate
,   setAnimation
,   setAnimationSpeed
,   withCollisioners
,   setCollisioners
    -- * Rendering
,   goRender
    -- * Collision
,   goCollide
)
where


import Spear.Collision as Col
import Spear.GLSL
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
    { gameStyle    :: !GameStyle
    , renderer     :: !(Either StaticModelRenderer AM.AnimatedModelRenderer)
    , collisioners :: ![Collisioner]
    , transform    :: !M3.Matrix3
    }


instance S2.Spatial2 GameObject where
    
    move v go = go
        { collisioners = fmap (Col.move v) $ collisioners go
        , transform  = M3.translv v * transform go
        }
    
    moveFwd s go =
        let m = transform go
            v = V2.scale s $ M3.forward m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
            , transform = M3.translv v * m
            }
    
    moveBack s go =
        let m = transform go
            v = V2.scale (-s) $ M3.forward m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
            , transform = M3.translv v * m
            }
    
    strafeLeft s go =
        let m = transform go
            v = V2.scale (-s) $ M3.right m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
            , transform = M3.translv v * m
            }
    
    strafeRight s go =
        let m = transform go
            v = V2.scale s $ M3.right m
        in go
            { collisioners = fmap (Col.move v) $ collisioners go
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
      -> [Collisioner]
      -> M3.Matrix3
      -> GameObject

goNew style (Left smr) cols transf =
    GameObject style (Left $ SM.staticModelRenderer smr) cols transf

goNew style (Right amr) cols transf =
    GameObject style (Right $ AM.animatedModelRenderer 1 amr) cols transf


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


-- | Return the game object's number of collisioners.
numCollisioners :: GameObject -> Int
numCollisioners = length . collisioners


-- | Manipulate the game object's collisioners.
withCollisioners :: GameObject -> ([Collisioner] -> [Collisioner]) -> GameObject
withCollisioners go f = go { collisioners = f $ collisioners go }


-- | Set the game object's collisioners.
setCollisioners :: GameObject -> [Collisioner] -> GameObject
setCollisioners go cols = go { collisioners = cols }


-- | Get the game object's ith bounding box.
goAABB :: Int -> GameObject -> AABB
goAABB i go = goAABB' $ (collisioners go) !! i

goAABB' col = case col of
    (AABBCol box) -> box
    (CircleCol circle) -> aabbFromCircle circle


-- | Get the game object's bounding boxes.
goAABBs :: GameObject -> [AABB]
goAABBs = fmap goAABB' . collisioners


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
