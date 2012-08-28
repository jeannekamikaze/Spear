module Spear.Scene.GameObject
(
    GameObject
,   CollideGO
,   UpdateGO
    -- * Construction
,   goNew
        -- * Accessors
,   goData
    -- * Manipulation
,   goUpdate
,   withGO
    -- * Rendering
,   goRender
)
where


import Spear.Collision.Collision
import Spear.Collision.Collisioner
import Spear.Math.AABB
import Spear.Render.AnimatedModel as AM
import Spear.Render.Program
import Spear.Render.StaticModel as SM

import Data.List (foldl')


-- | Collide a game object.
type CollideGO a
    =  GameObject a -- ^ Collider
    -> GameObject a -- ^ Old game object
    -> GameObject a -- ^ New game object

-- | Update a game object.
type UpdateGO a = Float -> GameObject a -> GameObject a


-- | An object in the game scene.
data GameObject a = GameObject
    { renderer    :: !(Either StaticModelRenderer AnimatedModelRenderer)
    , collisioner :: !Collisioner
    , goData      :: !a
    , goUpdt      :: UpdateGO a
    , goCol       :: CollideGO a
    }


-- | Create a new game object.
goNew :: Either StaticModelResource AnimatedModelResource
      -> Collisioner -> a -> UpdateGO a -> CollideGO a -> GameObject a

goNew (Left  smr) = GameObject (Left  $ staticModelRenderer smr)
goNew (Right amr) = GameObject (Right $ animatedModelRenderer amr)


-- | Render the game object.
goRender :: StaticProgramUniforms -> AnimatedProgramUniforms -> GameObject a -> IO ()
goRender spu apu go =
    case renderer go of
        Left smr  -> SM.render spu smr
        Right amr -> AM.render apu amr


-- | Update the game object.
goUpdate :: Float -> GameObject a -> GameObject a
goUpdate dt go =
    case renderer go of
        Left smr  -> goUpdt go dt $ go
        Right amr -> goUpdt go dt $ go { renderer = Right $ AM.update dt amr }


-- | Apply the given function to the game object's data.
withGO :: GameObject a -> (a -> a) -> GameObject a
withGO go f = go { goData = f $ goData go }


-- | Collide the game object with the given list of game objects.
goCollide :: [GameObject a] -> GameObject a -> GameObject a
goCollide gos go = foldl' collide' go gos
    where
        collide' go1 go2 = goCol go1 go2 go1


-- | Get the object's bounding box.
goAABB :: GameObject a -> AABB
goAABB go =
    case collisioner go of
        (AABBCol box) -> box
        (CircleCol circle) -> aabbFromCircle circle
