module Spear.Collision.Collisioner
(
    Collisioner(..)
,   CollisionType(..)
,   aabbCollisioner
,   sphereCollisioner
,   buildAABB
,   collide
,   move
)
where


import Spear.Collision.Collision as C
import Spear.Collision.Types
import Spear.Math.AABB
import Spear.Math.Circle
import Spear.Math.Vector2


-- | A collisioner component.
data Collisioner
    -- | An axis-aligned bounding box.
    = AABBCol { getBox :: {-# UNPACK #-} !AABB }
    -- | A bounding sphere.
    | CircleCol { getCircle :: {-# UNPACK #-} !Circle }


-- | Create a 'Collisioner' from the specified box.  
aabbCollisioner :: AABB -> Collisioner
aabbCollisioner = AABBCol


-- | Create a 'Collisioner' from the specified circle.
sphereCollisioner :: Circle -> Collisioner
sphereCollisioner = CircleCol


-- | Create the minimal 'AABB' fully containing the specified collisioners.
buildAABB :: [Collisioner] -> AABB
buildAABB cols = aabb $ generatePoints cols


-- | Create the minimal 'AABB' collisioner fully containing the specified circle.
boxFromSphere :: Circle -> Collisioner
boxFromSphere = AABBCol . aabbFromCircle


generatePoints :: [Collisioner] -> [Vector2]
generatePoints = foldr generate []
    where
        generate (AABBCol (AABB min max)) acc = p1:p2:p3:p4:p5:p6:p7:p8:acc
            where
                p1 = vec2 (x min) (y min)
                p2 = vec2 (x min) (y min)
                p3 = vec2 (x min) (y max)
                p4 = vec2 (x min) (y max)
                p5 = vec2 (x max) (y min)
                p6 = vec2 (x max) (y min)
                p7 = vec2 (x max) (y max)
                p8 = vec2 (x max) (y max)
    
        generate (CircleCol (Circle c r)) acc = p1:p2:p3:p4:acc
            where
                p1 = c + unitx * (vec2 r r)
                p2 = c - unitx * (vec2 r r)
                p3 = c + unity * (vec2 r r)
                p4 = c - unity * (vec2 r r)


-- | Collide the given collisioners.
collide :: Collisioner -> Collisioner -> CollisionType
collide (AABBCol box1) (AABBCol box2)    = collideBox    box1 box2
collide (CircleCol s1) (CircleCol s2)    = collideSphere s1 s2
collide (AABBCol box) (CircleCol sphere) = collideBox    box sphere
collide (CircleCol sphere) (AABBCol box) = collideSphere sphere box


-- | Move the collisioner.
move :: Vector2 -> Collisioner -> Collisioner
move v (AABBCol (AABB min max)) = AABBCol (AABB (min+v) (max+v))
move v (CircleCol (Circle c r)) = CircleCol (Circle (c+v) r)
