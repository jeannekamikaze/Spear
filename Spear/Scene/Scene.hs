module Spear.Scene.Scene
(
    -- * Data types
    Scene
    -- * Construction
,   listScene
    -- * Insertion and deletion
,   add
,   addl
,   remove
,   Spear.Scene.Scene.filter
    -- * Queries
,   find
    -- * Update and render
,   update
,   updateM
,   collide
,   collideM
,   render
)
where


import Spear.Collision.AABB
import Spear.Collision.Types
import Spear.Game (Game)
import Spear.Math.Octree as Octree

import Control.Applicative ((<*>))
import Control.Monad (foldM)
import Data.Foldable as F (foldl', mapM_)
import Data.Functor ((<$>))
import qualified Data.List as L (delete, filter, find)


data Scene obj =
    ListScene
    { objects        :: [obj]
    }
    |
    OctreeScene
    { collideAABB    :: obj -> AABB -> CollisionType
    , world          :: Octree obj
    }


-- | Create a list-based scene.
listScene :: [obj] -> Scene obj
listScene = ListScene


-- Create an octree-based scene.
--octreeScene :: (obj -> AABB -> CollisionType) -> (obj -> AABB) -> [obj] -> Scene obj msg
--octreeScene collide getAABB objs = OctreeScene [] collide $ makeOctree


-- | Add a game object to the given 'Scene'.
add :: Scene obj -> obj -> Scene obj
add (scene@ListScene {})   o = scene { objects = o : objects scene }
add (scene@OctreeScene {}) o = scene { world = insert (collideAABB scene) (world scene) o }


-- | Add a list of game objects to the given 'Scene'.
addl :: Scene obj -> [obj] -> Scene obj
addl (scene@ListScene {})   l = scene { objects = l ++ objects scene }
addl (scene@OctreeScene {}) l = scene { world = insertl (collideAABB scene) (world scene) l }


-- | Remove a game object from the given 'Scene'.
remove :: Eq obj => Scene obj -> obj -> Scene obj
remove (scene@ListScene {})   o = scene { objects = L.delete o (objects scene) }
--remove (scene@OctreeScene {}) o =


-- | Remove those game objects that do not satisfy the given predicate from the 'Scene'.
filter :: (obj -> Bool) -> Scene obj -> Scene obj
filter pred (scene@ListScene {}) = scene { objects = L.filter pred (objects scene) }


-- | Search for an object in the 'Scene'.
find :: (obj -> Bool) -> Scene obj -> Maybe obj
find pred (scene@ListScene {}) = L.find pred $ objects scene


type Update obj = obj -> obj


-- | Update the given scene.
update :: (obj -> obj) -> Scene obj -> Scene obj
update updt (scene@ListScene {})   = scene { objects = fmap updt $ objects scene }
update updt (scene@OctreeScene {}) = scene { world   = Octree.map (collideAABB scene) updt $ world scene }


-- | Update the given scene.
updateM :: Monad m => (obj -> m obj) -> Scene obj -> m (Scene obj)
updateM updt scene@ListScene {} = mapM updt (objects scene) >>= return . ListScene


{-update' :: (obj -> (obj, [a])) -> Scene obj -> (Scene obj, [a])

update' updt (scene@ListScene {}) =
    let (objs, msgs) = unzip . fmap updt $ objects scene
    in (scene { objects = objs }, concat msgs)-}


-- | Perform collisions.
collide :: (obj -> obj -> obj) -> Scene obj -> Scene obj

collide col scene@ListScene {} =
    let objs   = objects scene
        objs'  = fmap col' objs
        col' o = foldl' col o objs
    in
        scene { objects = objs' }

collide col scene@OctreeScene {} =
    scene { world = gmap (collideAABB scene) col $ world scene }


-- | Perform collisions.
collideM :: Monad m => (obj -> obj -> m obj) -> Scene obj -> m (Scene obj)
collideM col scene@ListScene {} =
    let objs = objects scene

        col' o = foldM f o objs
        f o p  = col o p

        objs' = sequence . fmap col' $ objs
    in
        objs' >>= return . ListScene


{-collide' :: (obj -> obj -> (obj, [a])) -> Scene obj -> (Scene obj, [a])

collide' col scene@ListScene {} =
    let objs = objects scene

        --col' :: obj -> (obj, [a])
        col' o = foldl' f (o, []) objs

        --f :: (obj, [a]) -> obj -> (obj, [a])
        f (o, msgs) p  = let (o', msgs') = col o p in (o', msgs' ++ msgs)

        (objs', msgs) = let (os, ms) = (unzip . fmap col' $ objs) in (os, concat ms)
    in
        (scene { objects = objs' }, msgs)-}


-- | Render the given 'Scene'.
render :: (obj -> Game s ()) -> Scene obj -> Game s ()
render rend (scene@ListScene {})   = Prelude.mapM_ rend $ objects scene
render rend (scene@OctreeScene {}) = F.mapM_ rend $ world scene
