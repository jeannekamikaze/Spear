module Spear.Collision.Collisioner
(
    Collisioner(..)
,   CollisionType(..)
,   aabbCollisioner
,   sphereCollisioner
,   buildAABB
,   collide
)
where


import Spear.Math.Vector3 as Vector
import Spear.Collision.AABB as Box
import Spear.Collision.Sphere as Sphere
import Spear.Collision.Collision as C
import Spear.Collision.Types


-- | A collisioner component.
data Collisioner
    -- | An axis-aligned bounding box.
    = AABBCol { getBox :: !AABB }
    -- | A bounding sphere.
    | SphereCol { getSphere :: !Sphere }


-- | Create a 'Collisioner' from the specified 'AABB'.  
aabbCollisioner :: AABB -> Collisioner
aabbCollisioner = AABBCol


-- | Create a 'Collisioner' from the specified 'BSphere'.
sphereCollisioner :: Sphere -> Collisioner
sphereCollisioner = SphereCol


-- | Create the minimal 'AABB' fully containing the specified collisioners.
buildAABB :: [Collisioner] -> AABB
buildAABB cols = aabb $ Spear.Collision.Collisioner.generatePoints cols


-- | Create the minimal 'AABB' collisioner fully containing the specified 'BSphere'.
boxFromSphere :: Sphere.Sphere -> Collisioner
boxFromSphere = AABBCol . aabbFromSphere


generatePoints :: [Collisioner] -> [Vector3]
generatePoints = foldr generate []
    where
        generate (AABBCol (AABB min max)) acc = p1:p2:p3:p4:p5:p6:p7:p8:acc
            where
                p1 = vec3 (x min) (y min) (z min)
                p2 = vec3 (x min) (y min) (z max)
                p3 = vec3 (x min) (y max) (z min)
                p4 = vec3 (x min) (y max) (z max)
                p5 = vec3 (x max) (y min) (z min)
                p6 = vec3 (x max) (y min) (z max)
                p7 = vec3 (x max) (y max) (z min)
                p8 = vec3 (x max) (y max) (z max)
    
        generate (SphereCol (Sphere c r)) acc = p1:p2:p3:p4:p5:p6:acc
            where
                p1 = c + unitX * (vec3 r r r)
                p2 = c - unitX * (vec3 r r r)
                p3 = c + unitY * (vec3 r r r)
                p4 = c - unitY * (vec3 r r r)
                p5 = c + unitZ * (vec3 r r r)
                p6 = c - unitZ * (vec3 r r r)


-- | Collide the given collisioners.
collide :: Collisioner -> Collisioner -> CollisionType
collide (AABBCol box1) (AABBCol box2)    = collideBox    box1 box2
collide (SphereCol s1) (SphereCol s2)    = collideSphere s1 s2
collide (AABBCol box) (SphereCol sphere) = collideBox    box sphere
collide (SphereCol sphere) (AABBCol box) = collideSphere sphere box
