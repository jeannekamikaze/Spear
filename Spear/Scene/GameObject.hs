module Spear.Scene.GameObject
(
    GameObject
,   GameStyle(..)
,   AM.AnimationSpeed
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
    , renderer     :: !(Either StaticModelRenderer AM.AnimatedModelRenderer)
    , collisioners :: ![Collisioner]
    , transform    :: !M3.Matrix3
    , axis         :: Vector3
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
            fwd      = V2.normalise $ p - position
            r        = perp fwd
        in
            go
            { transform = M3.transform r fwd position
            , angle = acos $ r `V2.dot` V2.unitx
            }


-- | Create a new game object.
goNew :: GameStyle
      -> Either StaticModelResource AM.AnimatedModelResource
      -> [Collisioner]
      -> M3.Matrix3 -- ^ Transform
      -> Vector3 -- ^ Axis of rotation
      -> GameObject

goNew style (Left smr) cols transf axis =
    GameObject style (Left $ SM.staticModelRenderer smr) cols transf axis 0

goNew style (Right amr) cols transf axis =
    GameObject style (Right $ AM.animatedModelRenderer 1 amr) cols transf axis 0


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
go3Dtransform :: GameObject -> M4.Matrix4
go3Dtransform go = rpgTransform 0 (angle go) (axis go) . S2.transform $ go


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


-- | Set the game object's collisioners.
setCollisioners :: GameObject -> [Collisioner] -> GameObject
setCollisioners go cols = go { collisioners = cols }


-- | Manipulate the game object's collisioners.
withCollisioners :: GameObject -> ([Collisioner] -> [Collisioner]) -> GameObject
withCollisioners go f = go { collisioners = f $ collisioners go }


-- | Render the game object.
goRender :: StaticProgram -> AnimatedProgram -> Cam.Camera -> GameObject -> IO ()
goRender sprog aprog cam go =
    let spu = staticProgramUniforms sprog
        apu = animatedProgramUniforms aprog
        mat = S2.transform go
        style = gameStyle go
        axis' = axis go
        a   = angle go
    in case renderer go of
        Left smr  ->
            goRender' style a axis' sprog spu mat cam (SM.bind spu smr) (SM.render spu smr)
        Right amr ->
            goRender' style a axis' aprog apu mat cam (AM.bind apu amr) (AM.render apu amr)


type Bind = IO ()

type Render = IO ()


goRender' :: (ProgramUniforms u, Program p)
          => GameStyle
          -> Float
          -> Vector3
          -> p
          -> u
          -> M3.Matrix3
          -> Cam.Camera
          -> Bind
          -> Render
          -> IO ()
goRender' style a axis prog uniforms model cam bindRenderer render =
    let view  = M4.inverseTransform $ Cam.transform cam
        modelview = case style of
            RPG -> view * rpgTransform 0 a axis model
            PLT -> view * pltTransform model
        normalmat = fastNormalMatrix modelview
    in do
        useProgram . program $ prog
        uniformMat4 (projLoc uniforms) $ Cam.projection cam
        uniformMat4 (modelviewLoc uniforms) modelview
        uniformMat3 (normalmatLoc uniforms) normalmat
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
