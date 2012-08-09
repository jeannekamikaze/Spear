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
,   withBody
,   objectTransform
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
import Spear.Sys.Store


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
    { bodies  :: Store Object -- ^ Collection of objects.
    , gravity :: Vector3 -- ^ World gravity.
    }


-- | Create an empty world.
emptyWorld :: World
emptyWorld = World emptyStore $ vec3 0 (-9.8) 0


-- | Create a new object.
newObject :: RigidBody -> Collisioner -> World -> (ObjectID, World)
newObject body collisioner world =
    let (index, bodies') = store (Object body collisioner []) $ bodies world 
    in (ObjectID index, world { bodies = bodies' })


-- | Remove the object specified by the given object ID.
deleteObject :: ObjectID -> World -> World
deleteObject (ObjectID i) world = world { bodies = bodies' }
    where
        bodies' = storeFree i $ bodies world


-- | Modify the object identified by the given object ID.
withBody :: ObjectID -> World -> (RigidBody -> RigidBody) -> World
withBody (ObjectID index) world f = world { bodies = bodies' }
    where
        bodies' = withElement index (bodies world) $ \obj -> obj { body = f $ body obj }


-- | Get the transform of the object identified by the given object ID.
objectTransform :: World -> ObjectID -> Matrix4
objectTransform world (ObjectID i) = transform . body . fromJust $ (element i $ bodies world)


-- | Add the given force to the forces acting on the object identified by the given object ID.
setForces :: [Force] -> ObjectID -> World -> World
setForces fs (ObjectID i) world = world { bodies = bodies' }
    where
        bodies' = withElement i (bodies world) $ \obj -> obj { forces = fs }


-- | Set the world's gravity.
setGravity :: Vector3 -> World -> World
setGravity g world = world { gravity = g }


-- | Update the world.
updateWorld :: Dt -> World -> World
updateWorld dt world = world { bodies = fmap updateObject $ bodies world }
    where        
        updateObject (Object body collisioner forces) = Object body' collisioner' forces
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


{--- | Test for potential collisions.
--
-- Returns a new world and a list of colliding pairs of objects.
--testCollisions :: World -> (World, [(ObjectID, ObjectID)])-}

