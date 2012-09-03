module Spear.Collision
(
    -- * Collision tests
    CollisionType(..)
,   Collisionable(..)
    -- * Collisioners
,   Collisioner(..)
    -- ** Construction
,   aabbCollisioner
,   circleCollisioner
,   boxFromCircle
,   buildAABB
,   mkCols
    -- ** Collision test
,   collide
    -- ** Manipulation
,   move
    -- * Helpers
,   aabbFromCircle
)
where


import Spear.Assets.Model
import Spear.Math.AABB
import Spear.Math.Circle
import qualified Spear.Math.Matrix4 as M4
import Spear.Math.Plane
import Spear.Math.Vector2
import qualified Spear.Math.Vector3 as V3


-- | Encodes several collision situations.
data CollisionType = NoCollision | Collision | FullyContains | FullyContainedBy
    deriving (Eq, Show)


class Collisionable a where
    collideBox    :: AABB   -> a -> CollisionType
    collideCircle :: Circle -> a -> CollisionType
    getAABB       :: a -> AABB
    getCircle     :: a -> Circle


instance Collisionable AABB where
    
    collideBox box1@(AABB min1 max1) box2@(AABB min2 max2)
        | (x max1) < (x min2) = NoCollision
        | (x min1) > (x max2) = NoCollision
        | (y max1) < (y min2) = NoCollision
        | (y min1) > (y max2) = NoCollision
        | box1 `aabbpt` min2 && box1 `aabbpt` max2 = FullyContains
        | box2 `aabbpt` min1 && box2 `aabbpt` max1 = FullyContainedBy
        | otherwise = Collision
    
    collideCircle circle@(Circle c r) aabb@(AABB min max)
        | test == FullyContains || test == FullyContainedBy = test
        | normSq (c - boxC) > (l + r)^2 = NoCollision
        | otherwise = Collision
            where
                test = aabb `collideBox` aabbFromCircle circle
                boxC = min + (max-min)/2
                l = norm $ min + (vec2 (x boxC) (y min)) - min
    
    getAABB = id
   
    getCircle = circleFromAABB


instance Collisionable Circle where
    
    collideBox box circle = case collideCircle circle box of
        FullyContains -> FullyContainedBy
        FullyContainedBy -> FullyContains
        x -> x
    
    collideCircle s1@(Circle c1 r1) s2@(Circle c2 r2)
        | distance_centers <= sub_radii = if (r1 > r2) then FullyContains else FullyContainedBy
        | distance_centers <= sum_radii = Collision
        | otherwise = NoCollision
        where
            distance_centers = normSq $ c1 - c2
            sum_radii    = (r1 + r2)^2
            sub_radii    = (r1 - r2)^2
    
    getAABB = aabbFromCircle
    
    getCircle = id


instance Collisionable Collisioner where
    
    collideBox box (AABBCol self) = collideBox box self
    collideBox box (CircleCol self) = collideBox box self 
    
    collideCircle circle (AABBCol self) = collideCircle circle self
    collideCircle circle (CircleCol self) = collideCircle circle self
    
    getAABB (AABBCol box) = box
    getAABB (CircleCol c) = aabbFromCircle c
    
    getCircle (AABBCol box) = circleFromAABB box
    getCircle (CircleCol c) = c
    
    


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
    = AABBCol {-# UNPACK #-} !AABB
    -- | A bounding circle.
    | CircleCol {-# UNPACK #-} !Circle


-- | Create a collisioner from the specified box.  
aabbCollisioner :: AABB -> Collisioner
aabbCollisioner = AABBCol


-- | Create a collisioner from the specified circle.
circleCollisioner :: Circle -> Collisioner
circleCollisioner = CircleCol


-- | Create the minimal AABB collisioner fully containing the specified circle.
boxFromCircle :: Circle -> Collisioner
boxFromCircle = AABBCol . aabbFromCircle


-- | Create the minimal AABB fully containing the specified collisioners.
buildAABB :: [Collisioner] -> AABB
buildAABB cols = aabb $ generatePoints cols


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


-- | Compute collisioners in view space from the given 3D AABB.
mkCols :: M4.Matrix4 -- ^ Modelview matrix
       -> Box
       -> [Collisioner]
mkCols modelview (Box (Vec3 xmin ymin zmin) (Vec3 xmax ymax zmax)) =
    let
        toVec2 v = vec2 (V3.x v) (V3.y v)
        p1   = toVec2 $ modelview `M4.mulp` V3.vec3 xmin ymin zmax
        p2   = toVec2 $ modelview `M4.mulp` V3.vec3 xmax ymin zmin
        p3   = toVec2 $ modelview `M4.mulp` V3.vec3 xmax ymax zmin
        col1 = AABBCol $ AABB p1 p2
        col2 = AABBCol $ AABB p1 p3
    in
        [col1, col2]


-- | Collide the given collisioners.
collide :: Collisioner -> Collisioner -> CollisionType
collide (AABBCol box1) (AABBCol box2)    = collideBox    box1 box2
collide (CircleCol s1) (CircleCol s2)    = collideCircle s1 s2
collide (AABBCol box) (CircleCol circle) = collideBox    box circle
collide (CircleCol circle) (AABBCol box) = collideCircle circle box


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


-- | Create the minimal circle fully containing the specified box.
circleFromAABB :: AABB -> Circle
circleFromAABB (AABB min max) = Circle c r
    where
        c = scale 0.5 (min + max)
        r = norm . scale 0.5 $ max - min
