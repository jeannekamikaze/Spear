module Spear.Math.Collision
(
    CollisionType(..)
    -- * 2D Collision
,   Collisionable2(..)
,   Collisioner2(..)
    -- ** Construction
,   aabb2Collisioner
,   circleCollisioner
,   mkCols
    -- ** Collision test
,   collide
    -- ** Manipulation
,   move
    -- ** Helpers
,   buildAABB2
,   aabb2FromCircle
,   circleFromAABB2
    -- * 3D Collision
,   Collisionable3(..)
    -- ** Helpers
,   aabb3FromSphere
)
where

import Spear.Assets.Model
import Spear.Math.AABB
import Spear.Math.Circle
import qualified Spear.Math.Matrix4 as M4
import Spear.Math.Plane
import Spear.Math.Sphere
import Spear.Math.Vector

import Data.List (foldl')

data CollisionType = NoCollision | Collision | FullyContains | FullyContainedBy
     deriving (Eq, Show)

-- 2D collision

class Collisionable2 a where

      -- | Collide the object with an AABB.
      collideAABB2 :: AABB2 -> a -> CollisionType

      -- | Collide the object with a circle.
      collideCircle :: Circle -> a -> CollisionType

instance Collisionable2 AABB2 where

    collideAABB2 box1@(AABB2 min1 max1) box2@(AABB2 min2 max2)
        | (x max1) < (x min2) = NoCollision
        | (x min1) > (x max2) = NoCollision
        | (y max1) < (y min2) = NoCollision
        | (y min1) > (y max2) = NoCollision
        | box1 `aabb2pt` min2 && box1 `aabb2pt` max2 = FullyContains
        | box2 `aabb2pt` min1 && box2 `aabb2pt` max1 = FullyContainedBy
        | otherwise = Collision

    collideCircle circle@(Circle c r) aabb@(AABB2 min max)
        | test == FullyContains || test == FullyContainedBy = test
        | normSq (c - boxC) > (l + r)^2 = NoCollision
        | otherwise = Collision
            where
                test = collideAABB2 aabb $ aabb2FromCircle circle
                boxC = min + (max-min)/2
                l = norm $ min + (vec2 (x boxC) (y min)) - min

instance Collisionable2 Circle where

    collideAABB2 box circle = case collideCircle circle box of
        FullyContains    -> FullyContainedBy
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

instance Collisionable2 Collisioner2 where

    collideAABB2 box (AABB2Col self)  = collideAABB2 box self
    collideAABB2 box (CircleCol self) = collideAABB2 box self

    collideCircle circle (AABB2Col self)  = collideCircle circle self
    collideCircle circle (CircleCol self) = collideCircle circle self

aabbPoints :: AABB2 -> [Vector2]
aabbPoints (AABB2 min max) = [p1,p2,p3,p4,p5,p6,p7,p8]
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
data Collisioner2
    -- | An axis-aligned bounding box.
    = AABB2Col {-# UNPACK #-} !AABB2
    -- | A bounding circle.
    | CircleCol {-# UNPACK #-} !Circle


-- | Create a collisioner from the specified box.
aabb2Collisioner :: AABB2 -> Collisioner2
aabb2Collisioner = AABB2Col

-- | Create a collisioner from the specified circle.
circleCollisioner :: Circle -> Collisioner2
circleCollisioner = CircleCol

-- | Compute AABB collisioners in view space from the given AABB.
mkCols :: M4.Matrix4 -- ^ Modelview matrix
       -> Box
       -> [Collisioner2]
mkCols modelview (Box (Vec3 xmin ymin zmin) (Vec3 xmax ymax zmax)) =
    let
        toVec2 v = vec2 (x v) (y v)
        p1   = toVec2 $ modelview `M4.mulp` vec3 xmin ymin zmax
        p2   = toVec2 $ modelview `M4.mulp` vec3 xmax ymin zmin
        p3   = toVec2 $ modelview `M4.mulp` vec3 xmax ymax zmin
        col1 = AABB2Col $ AABB2 p1 p2
        col2 = AABB2Col $ AABB2 p1 p3
    in
        [col1, col2]

-- | Create the minimal AABB fully containing the specified collisioners.
buildAABB2 :: [Collisioner2] -> AABB2
buildAABB2 cols = aabb2 $ generatePoints cols

-- | Create the minimal box fully containing the specified circle.
aabb2FromCircle :: Circle -> AABB2
aabb2FromCircle (Circle c r) = AABB2 bot top
    where
        bot = c - (vec2 r r)
        top = c + (vec2 r r)

-- | Create the minimal circle fully containing the specified box.
circleFromAABB2 :: AABB2 -> Circle
circleFromAABB2 (AABB2 min max) = Circle c r
    where
        c = scale 0.5 (min + max)
        r = norm . scale 0.5 $ max - min

generatePoints :: [Collisioner2] -> [Vector2]
generatePoints = foldl' generate []
    where
        generate acc (AABB2Col (AABB2 pmin pmax)) = p1:p2:p3:p4:p5:p6:p7:p8:acc
            where
                p1 = vec2 (x pmin) (y pmin)
                p2 = vec2 (x pmin) (y pmin)
                p3 = vec2 (x pmin) (y pmax)
                p4 = vec2 (x pmin) (y pmax)
                p5 = vec2 (x pmax) (y pmin)
                p6 = vec2 (x pmax) (y pmin)
                p7 = vec2 (x pmax) (y pmax)
                p8 = vec2 (x pmax) (y pmax)

        generate acc (CircleCol (Circle c r)) = p1:p2:p3:p4:acc
            where
                p1 = c + unitx2 * (vec2 r r)
                p2 = c - unitx2 * (vec2 r r)
                p3 = c + unity2 * (vec2 r r)
                p4 = c - unity2 * (vec2 r r)

-- | Collide the given collisioners.
collide :: Collisioner2 -> Collisioner2 -> CollisionType
collide (AABB2Col box1) (AABB2Col box2)   = collideAABB2  box1   box2
collide (AABB2Col box) (CircleCol circle) = collideAABB2  box    circle
collide (CircleCol s1) (CircleCol s2)     = collideCircle s1     s2
collide (CircleCol circle) (AABB2Col box) = collideCircle circle box

-- | Move the collisioner.
move :: Vector2 -> Collisioner2 -> Collisioner2
move v (AABB2Col (AABB2 min max)) = AABB2Col (AABB2 (min+v) (max+v))
move v (CircleCol (Circle c r)) = CircleCol (Circle (c+v) r)


-- 3D collision

class Collisionable3 a where

      -- | Collide the object with an AABB.
      collideAABB3 :: AABB3 -> a -> CollisionType

      -- | Collide the object with a sphere.
      collideSphere :: Sphere -> a -> CollisionType

instance Collisionable3 AABB3 where

         collideAABB3 box1@(AABB3 min1 max1) box2@(AABB3 min2 max2)
                      | (x max1) < (x min2) = NoCollision
                      | (x min1) > (x max2) = NoCollision
                      | (y max1) < (y min2) = NoCollision
                      | (y min1) > (y max2) = NoCollision
                      | box1 `aabb3pt` min2 && box1 `aabb3pt` max2 = FullyContains
                      | box2 `aabb3pt` min1 && box2 `aabb3pt` max1 = FullyContainedBy
                      | otherwise = Collision

         collideSphere sphere@(Sphere c r) aabb@(AABB3 min max)
                       | test == FullyContains || test == FullyContainedBy = test
                       | normSq (c - boxC) > (l + r)^2 = NoCollision
                       | otherwise = Collision
                         where
                            test = collideAABB3 aabb $ aabb3FromSphere sphere
                            boxC = min + v
                            l = norm v
                            v = (max-min)/2

instance Collisionable3 Sphere where

         collideAABB3 box sphere = case collideSphere sphere box of
                      FullyContains    -> FullyContainedBy
                      FullyContainedBy -> FullyContains
                      x -> x

         collideSphere s1@(Sphere c1 r1) s2@(Sphere c2 r2)
                       | distance_centers <= sub_radii =
                         if (r1 > r2) then FullyContains else FullyContainedBy
                       | distance_centers <= sum_radii = Collision
                       | otherwise = NoCollision
                         where
                            distance_centers = normSq $ c1 - c2
                            sum_radii = (r1 + r2)^2
                            sub_radii = (r1 - r2)^2

-- | Create the minimal box fully containing the specified sphere.
aabb3FromSphere :: Sphere -> AABB3
aabb3FromSphere (Sphere c r) = AABB3 bot top
    where
        bot = c - (vec3 r r r)
        top = c + (vec3 r r r)