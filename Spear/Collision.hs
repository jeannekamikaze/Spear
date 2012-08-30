module Spear.Collision
(
    -- * Collision tests
    CollisionType(..)
,   Collisionable(..)
    -- * Collisioners
,   Collisioner(..)
    -- ** Construction
,   aabbCollisioner
,   sphereCollisioner
,   buildAABB
    -- ** Collision test
,   collide
    -- ** Manipulation
,   move
    -- * Helpers
,   aabbFromCircle
)
where


import Spear.Math.AABB
import Spear.Math.Circle
import Spear.Math.Plane
import Spear.Math.Vector2


-- | Encodes several collision situations.
data CollisionType = NoCollision | Collision | FullyContains | FullyContainedBy
    deriving (Eq, Show)


class Collisionable a where
    collideBox    :: AABB   -> a -> CollisionType
    collideSphere :: Circle -> a -> CollisionType


instance Collisionable AABB where
    
    collideBox box1@(AABB min1 max1) box2@(AABB min2 max2)
        | min1 > max2   = NoCollision
        | max1 < min2   = NoCollision
        | box1 `aabbpt` min2 && box1 `aabbpt` max2 = FullyContains
        | box2 `aabbpt` min1 && box2 `aabbpt` max1 = FullyContainedBy
        | (x max1) < (x min2) = NoCollision
        | (x min1) > (x max2) = NoCollision
        | (y max1) < (y min2) = NoCollision
        | (y min1) > (y max2) = NoCollision
        | otherwise           = Collision
    
    collideSphere sphere@(Circle c r) aabb@(AABB min max)
        | test == FullyContains || test == FullyContainedBy = test
        | normSq (c - boxC) > (l + r)^2 = NoCollision
        | otherwise = Collision
            where
                test = aabb `collideBox` aabbFromCircle sphere
                boxC = min + (max-min)/2
                l = norm $ min + (vec2 (x boxC) (y min)) - min
    


instance Collisionable Circle where
    
    collideBox box sphere = case collideSphere sphere box of
        FullyContains -> FullyContainedBy
        FullyContainedBy -> FullyContains
        x -> x
    
    collideSphere s1@(Circle c1 r1) s2@(Circle c2 r2)
        | distance_centers <= sub_radii = if (r1 > r2) then FullyContains else FullyContainedBy
        | distance_centers <= sum_radii = Collision
        | otherwise = NoCollision
        where
            distance_centers = normSq $ c1 - c2
            sum_radii    = (r1 + r2)^2
            sub_radii    = (r1 - r2)^2


aabbPoints :: AABB -> [Vector2]
aabbPoints (AABB min max) = [p1,p2,p3,p4,p5,p6,p7,p8]
    where
        p1 = vec2 (x min) (y min)
        p2 = vec2 (x min) (y min)
        p3 = vec2 (x min) (y max)
        p4 = vec2 (x min) (y max)
        p5 = vec2 (x max) (y min)
        p6 = vec2 (x max) (y min)
        p7 = vec2 (x max) (y max)
        p8 = vec2 (x max) (y max)


-- | A collisioner component.
data Collisioner
    -- | An axis-aligned bounding box.
    = AABBCol { getBox :: {-# UNPACK #-} !AABB }
    -- | A bounding sphere.
    | CircleCol { getCircle :: {-# UNPACK #-} !Circle }


-- | Create a collisioner from the specified box.  
aabbCollisioner :: AABB -> Collisioner
aabbCollisioner = AABBCol


-- | Create a collisioner from the specified circle.
sphereCollisioner :: Circle -> Collisioner
sphereCollisioner = CircleCol


-- | Create the minimal AABB fully containing the specified collisioners.
buildAABB :: [Collisioner] -> AABB
buildAABB cols = aabb $ generatePoints cols


-- | Create the minimal AABB collisioner fully containing the specified circle.
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


-- | Create the minimal box fully containing the specified circle.
aabbFromCircle :: Circle -> AABB
aabbFromCircle (Circle c r) = AABB bot top
    where
        bot = c - (vec2 r r)
        top = c + (vec2 r r)
