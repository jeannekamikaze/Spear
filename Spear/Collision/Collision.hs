module Spear.Collision.Collision
(
    Collisionable(..)
,   aabbFromSphere
)
where


import Spear.Collision.Types
import Spear.Math.AABB
import Spear.Math.Sphere
import Spear.Math.Plane
import Spear.Math.Vector3


class Collisionable a where
    collideBox    :: AABB   -> a -> CollisionType
    collideSphere :: Sphere -> a -> CollisionType
    collidePlane  :: Plane  -> a -> CollisionType


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
        | (z max1) < (z min2) = NoCollision
        | (z min1) > (z max2) = NoCollision
        | otherwise           = Collision
    
    collideSphere sphere@(Sphere c r) aabb@(AABB min max)
        | test == FullyContains || test == FullyContainedBy = test
        | normSq (c - boxC) > (l + r)^2 = NoCollision
        | otherwise = Collision
            where
                test = aabb `collideBox` aabbFromSphere sphere
                boxC = min + (max-min)/2
                l = norm $ min + (vec3 (x boxC) (y min) (z min)) - min
    
    collidePlane pl aabb@(AABB {})
        | sameSide tests = NoCollision
        | otherwise      = Collision
        where
            tests = fmap (classify pl) $ aabbPoints aabb
            sameSide (x:xs) = all (==x) xs


instance Collisionable Sphere where
    
    collideBox box sphere = case collideSphere sphere box of
        FullyContains -> FullyContainedBy
        FullyContainedBy -> FullyContains
        x -> x
    
    collideSphere s1@(Sphere c1 r1) s2@(Sphere c2 r2)
        | distance_centers <= sub_radii = if (r1 > r2) then FullyContains else FullyContainedBy
        | distance_centers <= sum_radii = Collision
        | otherwise = NoCollision
        where
            distance_centers = normSq $ c1 - c2
            sum_radii    = (r1 + r2)^2
            sub_radii    = (r1 - r2)^2
    
    collidePlane pl s = NoCollision


aabbPoints :: AABB -> [Vector3]
aabbPoints (AABB min max) = [p1,p2,p3,p4,p5,p6,p7,p8]
    where
        p1 = vec3 (x min) (y min) (z min)
        p2 = vec3 (x min) (y min) (z max)
        p3 = vec3 (x min) (y max) (z min)
        p4 = vec3 (x min) (y max) (z max)
        p5 = vec3 (x max) (y min) (z min)
        p6 = vec3 (x max) (y min) (z max)
        p7 = vec3 (x max) (y max) (z min)
        p8 = vec3 (x max) (y max) (z max)


-- | Create the minimal AABB fully containing the specified Sphere.
aabbFromSphere :: Sphere -> AABB
aabbFromSphere (Sphere c r) = AABB bot top
    where
        bot = c - (vec3 r r r)
        top = c + (vec3 r r r)


-- | Create the minimal AABB fully containing the specified 'BoundingVolume's.
{-aabb :: [BoundingVolume] -> BoundingVolume
aabb = Spear.Collision.BoundingVolume.fromList BoundingBox . foldr generate []
    where
        generate (AABB min max) acc = p1:p2:p3:p4:p5:p6:p7:p8:acc
            where
                p1 = vec3 (x min) (y min) (z min)
                p2 = vec3 (x min) (y min) (z max)
                p3 = vec3 (x min) (y max) (z min)
                p4 = vec3 (x min) (y max) (z max)
                p5 = vec3 (x max) (y min) (z min)
                p6 = vec3 (x max) (y min) (z max)
                p7 = vec3 (x max) (y max) (z min)
                p8 = vec3 (x max) (y max) (z max)
        
        generate (Sphere c r) acc = p1:p2:p3:p4:p5:p6:acc
            where
                p1 = c + unitX * (vec3 r r r)
                p2 = c - unitX * (vec3 r r r)
                p3 = c + unitY * (vec3 r r r)
                p4 = c - unitY * (vec3 r r r)
                p5 = c + unitZ * (vec3 r r r)
                p6 = c - unitZ * (vec3 r r r)-}
