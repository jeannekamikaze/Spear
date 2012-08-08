module Spear.Physics.World
(
    module Spear.Physics.Types
    -- * Data types
,   World
,   ObjectID
    -- * Construction
,   emptyWorld
    -- * World operations
,   setGravity
,   updateWorld
    -- * Object operations
,   newObject
,   deleteObject
,   modifyObject
,   objectTransform
,   objectForces
,   setForces
)
where


import Spear.Collision.AABB
import Spear.Collision.Collisioner as C
import Spear.Collision.Sphere
import Spear.Math.Matrix4 (Matrix4)
import Spear.Math.Spatial
import Spear.Math.Vector3
import Spear.Physics.Rigid as Rigid
import Spear.Physics.Types

import Control.Monad.ST
import Data.Array as A
import Data.Array.ST
import Data.Maybe (fromJust)


-- | Uniquely identifies an object in a 'World'.
newtype ObjectID = ObjectID Int


data Object = Object
    { body        :: RigidBody
    , collisioner :: Collisioner
    , forces      :: [Vector3]
    }


-- | The world where physical bodies are simulated.
data World = World
    { bodies  :: Array Int (Maybe Object) -- ^ Collection of objects.
    , gravity :: Vector3 -- ^ World gravity.
    }


-- | Create an empty 'World'.
emptyWorld :: World
emptyWorld = World emptyArray defaultGravity
    where
        defaultGravity = vec3 0 (-9.8) 0
        emptyArray = listArray (0,0) []


-- | Create a new object.
newObject :: RigidBody -> Collisioner -> World -> (World, ObjectID)
newObject body collisioner world =
    let obj = (Object body collisioner [])
    in case emptySlot world of
        Just i  -> (insert i obj world, ObjectID i)
        Nothing -> append obj world


-- | Search for an empty slot in the given 'World'.
emptySlot :: World -> Maybe Int
emptySlot world = Nothing


-- | Insert the given 'Object' in the given 'World' at the given position.
insert :: Int -> Object -> World -> World
insert i obj world = world { bodies = bodies' }
    where
        bodies' = runSTArray $ do
            bs <- thaw $ bodies world
            writeArray bs i $ Just obj
            return bs


-- | Append the given object to the given 'World'.
--
-- The world's vectors are doubled in size to make future insertions faster.
append :: Object -> World -> (World, ObjectID)
append obj world = (world, ObjectID 0)


-- | Remove the object specified by the given 'ObjectID' from the given 'World'.
deleteObject :: ObjectID -> World -> World
deleteObject (ObjectID i) world = world { bodies = bodies' }
    where
        bodies' = runSTArray $ do
            bs <- thaw $ bodies world
            writeArray bs i Nothing
            return bs


-- | Modify the object identified by the given 'ObjectID' in the given 'World'.
modifyObject :: (RigidBody -> RigidBody) -> ObjectID -> World -> World
modifyObject f (ObjectID i) world = world { bodies = bodies' }
    where
        bodies' = runSTArray $ do
            bs  <- thaw $ bodies world
            obj <- readArray bs i
            writeArray bs i $ fmap (\obj -> obj { body = f $ body obj }) obj
            return bs


-- | Get the transform of the object identified by the given 'ObjectID'.
objectTransform :: World -> ObjectID -> Matrix4
objectTransform world (ObjectID i) = transform . body . fromJust $ bodies world ! i


-- | Get the forces acting on the object identified by the given 'ObjectID'.
objectForces :: World -> ObjectID -> [Force]
objectForces world (ObjectID i) = forces . fromJust $ bodies world ! i


-- | Add the given force to the forces acting on the object identified by the given 'ObjectID'.
setForces :: [Force] -> ObjectID -> World -> World
setForces fs (ObjectID i) world = world { bodies = bodies' }
    where
        bodies' = runSTArray $ do
            bs  <- thaw $ bodies world
            obj <- readArray bs i
            writeArray bs i $ fmap (\obj -> obj { forces = fs }) obj
            return bs


-- | Set the world's gravity.
setGravity :: Vector3 -> World -> World
setGravity g world = world { gravity = g }


-- | Update the 'World'.
updateWorld :: Dt -> World -> World
updateWorld dt world = world { bodies = bodies' }
    where
        bodies' = runSTArray $ do
            bs <- thaw $ bodies world
            mapArray updateObject bs
            return bs
        
        updateObject = fmap updateObject'
        updateObject' (Object body collisioner forces) = Object body' collisioner' forces
            where
                -- Forces acting on the body.
                forces' = scale (mass body) (gravity world) : forces
                
                -- Updated body.
                body' = Rigid.update forces dt body
                
                -- Center collisioner around the new body's center.
                collisioner' = center (Rigid.position body') collisioner
                
                -- Center the collisioner around the given point.
                center c (SphereCol (Sphere _ r)) = sphereCollisioner $ Sphere c r
                center c (AABBCol (AABB min max)) =
                    let v = (max - min) / 2
                        min' = c - v
                        max' = c + v
                    in
                        aabbCollisioner $ AABB min' max'


{--- | Test for potential collisions in the given 'World'.
--
-- Returns a new world and a list of colliding pairs of objects.
--testCollisions :: World -> (World, [(ObjectID, ObjectID)])-}

