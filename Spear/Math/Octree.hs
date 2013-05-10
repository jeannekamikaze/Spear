module Spear.Math.Octree
(
    Octree
,   makeOctree
,   clone
,   Spear.Math.Octree.insert
,   Spear.Math.Octree.map
,   gmap
)
where

import Spear.Math.AABB
import Spear.Math.Collision
import Spear.Math.Vector

import Control.Applicative ((<*>))
import Data.List
import Data.Functor
import Data.Monoid
import qualified Data.Foldable as F

-- | An octree.
data Octree e
    = Octree
    { root :: !AABB2
    , ents :: ![e]
    , c1   :: !(Octree e)
    , c2   :: !(Octree e)
    , c3   :: !(Octree e)
    , c4   :: !(Octree e)
    , c5   :: !(Octree e)
    , c6   :: !(Octree e)
    , c7   :: !(Octree e)
    , c8   :: !(Octree e)
    }
    |
    Leaf
    { root :: !AABB2
    , ents :: ![e]
    }

-- | Construct an octree using the specified AABB as the root and having the specified depth.
makeOctree :: Int -> AABB2 -> Octree e
makeOctree d root@(AABB2 min max)
    | d == 0    = Leaf root []
    | otherwise = Octree root [] c1 c2 c3 c4 c5 c6 c7 c8
    where
        boxes = subdivide root
        c1 = makeOctree (d-1) $ boxes !! 0
        c2 = makeOctree (d-1) $ boxes !! 1
        c3 = makeOctree (d-1) $ boxes !! 2
        c4 = makeOctree (d-1) $ boxes !! 3
        c5 = makeOctree (d-1) $ boxes !! 4
        c6 = makeOctree (d-1) $ boxes !! 5
        c7 = makeOctree (d-1) $ boxes !! 6
        c8 = makeOctree (d-1) $ boxes !! 7

subdivide :: AABB2 -> [AABB2]
subdivide (AABB2 min max) = [a1, a2, a3, a4, a5, a6, a7, a8]
    where
        v = (max-min) / 2
        c = vec2 (x min + x v) (y min + y v)
        a1 = AABB2 min c
        a2 = AABB2 ( vec2 (x min) (y min)) ( vec2 (x c)   (y c)  )
        a3 = AABB2 ( vec2 (x min) (y c)  ) ( vec2 (x c)   (y max))
        a4 = AABB2 ( vec2 (x min) (y c)  ) ( vec2 (x c)   (y max))
        a5 = AABB2 ( vec2 (x c)   (y min)) ( vec2 (x max) (y c)  )
        a6 = AABB2 ( vec2 (x c)   (y min)) ( vec2 (x max) (y c)  )
        a7 = AABB2 ( vec2 (x c)   (y c)  ) ( vec2 (x max) (y max))
        a8 = AABB2 c max

-- | Clone the structure of the octree. The new octree has no entities.
clone :: Octree e -> Octree e
clone (Leaf root ents) = Leaf root []
clone (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) = Octree root [] c1' c2' c3' c4' c5' c6' c7' c8'
    where
        c1' = clone c1
        c2' = clone c2
        c3' = clone c3
        c4' = clone c4
        c5' = clone c5
        c6' = clone c6
        c7' = clone c7
        c8' = clone c8

keep :: (e -> AABB2 -> CollisionType) -> AABB2 -> e -> Bool
keep testAABB2 aabb e = test == FullyContainedBy
    where test = e `testAABB2` aabb

-- | Insert a list of entities into the octree.
insert :: (e -> AABB2 -> CollisionType) -> Octree e -> [e] -> Octree e
insert testAABB2 octree es = octree' where (octree', _) = insert' testAABB2 es octree

insert' :: (e -> AABB2 -> CollisionType) -> [e] -> Octree e -> (Octree e, [e])

insert' testAABB2 es (Leaf root ents) = (Leaf root ents', outliers)
    where
        ents'       = ents ++ ents_kept
        ents_kept   = filter (keep testAABB2 root) es
        outliers    = filter (not . keep testAABB2 root) es

insert' testAABB2 es (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (Octree root ents' c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'        = ents ++ ents_kept
        new_ents     = es ++ ents1 ++ ents2 ++ ents3 ++ ents4 ++ ents5 ++ ents6 ++ ents7 ++ ents8
        ents_kept    = filter (keep testAABB2 root) new_ents
        outliers     = filter (not . keep testAABB2 root) new_ents
        (c1', ents1) = insert' testAABB2 es c1
        (c2', ents2) = insert' testAABB2 es c2
        (c3', ents3) = insert' testAABB2 es c3
        (c4', ents4) = insert' testAABB2 es c4
        (c5', ents5) = insert' testAABB2 es c5
        (c6', ents6) = insert' testAABB2 es c6
        (c7', ents7) = insert' testAABB2 es c7
        (c8', ents8) = insert' testAABB2 es c8

-- | Extract all entities from the octree. The resulting octree has no entities.
extract :: Octree e -> (Octree e, [e])
extract (Leaf root ents) = (Leaf root [], ents)
extract (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) = (Octree root [] c1' c2' c3' c4' c5' c6' c7' c8', ents')
    where
        (c1', ents1) = extract c1
        (c2', ents2) = extract c2
        (c3', ents3) = extract c3
        (c4', ents4) = extract c4
        (c5', ents5) = extract c5
        (c6', ents6) = extract c6
        (c7', ents7) = extract c7
        (c8', ents8) = extract c8
        ents' = ents ++ ents1 ++ ents2 ++ ents3 ++ ents4 ++ ents5 ++ ents6 ++ ents7 ++ ents8

-- | Apply the given function to the entities in the octree.
--
-- Entities that break out of their cell are reallocated appropriately.
map :: (e -> AABB2 -> CollisionType) -> (e -> e) -> Octree e -> Octree e
map testAABB2 f o =
    let (o', outliers) = map' testAABB2 f o
    in Spear.Math.Octree.insert testAABB2 o' outliers

map' :: (e -> AABB2 -> CollisionType) -> (e -> e) -> Octree e -> (Octree e, [e])

map' testAABB2 f (Leaf root ents) = (Leaf root ents_kept, outliers)
    where
        ents'       = fmap f ents
        ents_kept   = filter (keep testAABB2 root) ents'
        outliers    = filter (not . keep testAABB2 root) ents'

map' testAABB2 f (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (Octree root ents_kept c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'       = (fmap f ents) ++ out1 ++ out2 ++ out3 ++ out4 ++ out5 ++ out6 ++ out7 ++ out8
        ents_kept   = filter (keep testAABB2 root) ents'
        outliers    = filter (not . keep testAABB2 root) ents'
        (c1', out1) = map' testAABB2 f c1
        (c2', out2) = map' testAABB2 f c2
        (c3', out3) = map' testAABB2 f c3
        (c4', out4) = map' testAABB2 f c4
        (c5', out5) = map' testAABB2 f c5
        (c6', out6) = map' testAABB2 f c6
        (c7', out7) = map' testAABB2 f c7
        (c8', out8) = map' testAABB2 f c8


-- | Apply a function to the entity groups in the octree.
--
-- Entities that break out of their cell are reallocated appropriately.
gmap :: (e -> AABB2 -> CollisionType) -> (e -> e -> e) -> Octree e -> Octree e
gmap testAABB2 f o =
    let (o', outliers) = gmap' testAABB2 f o
    in Spear.Math.Octree.insert testAABB2 o' outliers

gmap' :: (e -> AABB2 -> CollisionType) -> (e -> e -> e) -> Octree e -> (Octree e, [e])

gmap' testAABB2 f (Leaf root ents) = (Leaf root ents_kept, outliers)
    where
        ents'       = f <$> ents <*> ents
        ents_kept   = filter (keep testAABB2 root) ents'
        outliers    = filter (not . keep testAABB2 root) ents'

gmap' testAABB2 f (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (Octree root ents_kept c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'= (f <$> ents <*> ents) ++ out1 ++ out2 ++ out3 ++ out4 ++ out5 ++ out6 ++ out7 ++ out8
        ents_kept   = filter (keep testAABB2 root) ents'
        outliers    = filter (not . keep testAABB2 root) ents'
        (c1', out1) = gmap' testAABB2 f c1
        (c2', out2) = gmap' testAABB2 f c2
        (c3', out3) = gmap' testAABB2 f c3
        (c4', out4) = gmap' testAABB2 f c4
        (c5', out5) = gmap' testAABB2 f c5
        (c6', out6) = gmap' testAABB2 f c6
        (c7', out7) = gmap' testAABB2 f c7
        (c8', out8) = gmap' testAABB2 f c8

instance Functor Octree where

    fmap f (Leaf root ents) = Leaf root $ fmap f ents

    fmap f (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
        Octree root (fmap f ents) c1' c2' c3' c4' c5' c6' c7' c8'
        where
            c1' = fmap f c1
            c2' = fmap f c2
            c3' = fmap f c3
            c4' = fmap f c4
            c5' = fmap f c5
            c6' = fmap f c6
            c7' = fmap f c7
            c8' = fmap f c8

instance F.Foldable Octree where

    foldMap f (Leaf root ents) = mconcat . fmap f $ ents

    foldMap f (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
        mconcat (fmap f ents) `mappend`
        c1' `mappend` c2' `mappend` c3' `mappend` c4' `mappend`
        c5' `mappend` c6' `mappend` c7' `mappend` c8'
        where
            c1' = F.foldMap f c1
            c2' = F.foldMap f c2
            c3' = F.foldMap f c3
            c4' = F.foldMap f c4
            c5' = F.foldMap f c5
            c6' = F.foldMap f c6
            c7' = F.foldMap f c7
            c8' = F.foldMap f c8
