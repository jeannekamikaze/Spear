module Spear.Collision.Collisioner
(
    Collisioner
,   CollisionType(..)
,   aabbCollisioner
,   sphereCollisioner
,   buildAABB
,   collide
)
where


import Spear.Math.Vector3 as Vector
import qualified Spear.Collision.AABB as Box
import qualified Spear.Collision.Sphere as Sphere
import Spear.Collision.Collision as C
import Spear.Collision.Types


-- | A collisioner component.
-- Wraps collision primitives so that one can collide them without being aware of
-- the underlying type.
data Collisioner
    -- | An axis-aligned bounding box.
    = AABB {getBox :: !(Box.AABB)}
    -- | A bounding sphere.
    | Sphere {getSphere :: !(Sphere.Sphere)
    }


-- | Create a 'Collisioner' from the specified 'AABB'.  
aabbCollisioner :: Box.AABB -> Collisioner
aabbCollisioner = AABB


-- | Create a 'Collisioner' from the specified 'BSphere'.
sphereCollisioner :: Sphere.Sphere -> Collisioner
sphereCollisioner = Sphere


-- | Create the minimal 'AABB' fully containing the specified collisioners.
buildAABB :: [Collisioner] -> Box.AABB
buildAABB cols = Box.aabb $ Spear.Collision.Collisioner.generatePoints cols


-- | Create the minimal 'AABB' collisioner fully containing the specified 'BSphere'.
boxFromSphere :: Sphere.Sphere -> Collisioner
boxFromSphere = AABB . aabbFromSphere


generatePoints :: [Collisioner] -> [Vector3]
generatePoints = foldr generate []
    where
        generate (AABB (Box.AABB min max)) acc = p1:p2:p3:p4:p5:p6:p7:p8:acc
            where
                p1 = vec3 (x min) (y min) (z min)
                p2 = vec3 (x min) (y min) (z max)
                p3 = vec3 (x min) (y max) (z min)
                p4 = vec3 (x min) (y max) (z max)
                p5 = vec3 (x max) (y min) (z min)
                p6 = vec3 (x max) (y min) (z max)
                p7 = vec3 (x max) (y max) (z min)
                p8 = vec3 (x max) (y max) (z max)
    
        generate (Sphere (Sphere.Sphere c r)) acc = p1:p2:p3:p4:p5:p6:acc
            where
                p1 = c + unitX * (vec3 r r r)
                p2 = c - unitX * (vec3 r r r)
                p3 = c + unitY * (vec3 r r r)
                p4 = c - unitY * (vec3 r r r)
                p5 = c + unitZ * (vec3 r r r)
                p6 = c - unitZ * (vec3 r r r)


-- | Collide the given collisioners.
collide :: Collisioner -> Collisioner -> CollisionType
collide (AABB box1) (AABB box2)    = collideBox    box1 box2
collide (Sphere s1) (Sphere s2)    = collideSphere s1 s2
collide (AABB box) (Sphere sphere) = collideBox    box sphere
collide (Sphere sphere) (AABB box) = collideSphere sphere box
