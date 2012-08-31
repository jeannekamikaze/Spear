module Spear.Scene.GameObject
(
    GameObject
,   GameStyle(..)
,   AM.AnimationSpeed
,   Rotation(..)
    -- * Construction
,   goNew
    -- * Accessors
,   currentAnimation
,   goAABB
,   goAABBs
,   go3Dtransform
,   numCollisioners
,   renderer
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

import Data.Fixed (mod')
import Data.List (foldl')


-- | Game style.
data GameStyle
    = RPG -- ^ RPG or RTS style game.
    | PLT -- ^ Platformer or space invaders style game.


-- | An object in the game scene.
data GameObject = GameObject
    { gameStyle    :: !GameStyle
    , rotation     :: !Rotation
    , renderer     :: !(Either StaticModelRenderer AM.AnimatedModelRenderer)
    , collisioners :: ![Collisioner]
    , transform    :: !M3.Matrix3
    , angle        :: Float
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
    
    rotate a go =
        go
        { transform = transform go * M3.rot a
        , angle = (angle go + a) `mod'` 360 
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
            fwd      = V2.normalise $ p - position
            r        = perp fwd
        in
            go
            { transform = M3.transform r fwd position
            , angle = acos $ r `V2.dot` V2.unitx
            }


-- | Create a new game object.
goNew :: GameStyle
      -> Rotation
      -> Either StaticModelResource AM.AnimatedModelResource
      -> [Collisioner]
      -> M3.Matrix3
      -> GameObject

goNew style rtype (Left smr) cols transf =
    GameObject style rtype (Left $ SM.staticModelRenderer smr) cols transf 0

goNew style rtype (Right amr) cols transf =
    GameObject style rtype (Right $ AM.animatedModelRenderer 1 amr) cols transf 0


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


-- | Get the game object's 3D transform.
go3Dtransform :: GameObject -> M4.Matrix4
go3Dtransform go = rpgTransform 0 (angle go) (rotation go) . S2.transform $ go


-- | Render the game object.
goRender :: StaticProgram -> AnimatedProgram -> Cam.Camera -> GameObject -> IO ()
goRender sprog aprog cam go =
    let spu = staticProgramUniforms sprog
        apu = animatedProgramUniforms aprog
        mat = S2.transform go
        style = gameStyle go
        rtype = rotation go
        a   = angle go
    in case renderer go of
        Left smr  ->
            goRender' style a rtype sprog spu mat cam (SM.bind spu smr) (SM.render spu smr)
        Right amr ->
            goRender' style a rtype aprog apu mat cam (AM.bind apu amr) (AM.render apu amr)


type Bind = IO ()

type Render = IO ()


goRender' :: (ProgramUniforms u, Program p)
          => GameStyle
          -> Float
          -> Rotation
          -> p
          -> u
          -> M3.Matrix3
          -> Cam.Camera
          -> Bind
          -> Render
          -> IO ()
goRender' style a rtype prog uniforms model cam bindRenderer render =
    let view  = M4.inverseTransform $ Cam.transform cam
        modelview = case style of
            RPG -> view * rpgTransform 0 a rtype model
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
