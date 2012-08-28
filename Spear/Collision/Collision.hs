module Spear.Collision.Collision
(
    Collisionable(..)
,   aabbFromCircle
)
where


import Spear.Collision.Types
import Spear.Math.AABB
import Spear.Math.Circle
import Spear.Math.Plane
import Spear.Math.Vector2


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


-- | Create the minimal box fully containing the specified circle.
aabbFromCircle :: Circle -> AABB
aabbFromCircle (Circle c r) = AABB bot top
    where
        bot = c - (vec2 r r)
        top = c + (vec2 r r)
