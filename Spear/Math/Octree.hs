module Spear.Math.Octree
(
    Octree
,   makeOctree
,   clone
,   Spear.Math.Octree.insert
,   insertl
,   Spear.Math.Octree.map
,   gmap
,   population
)
where

import Spear.Collision.AABB as AABB
import Spear.Collision.Types
import Spear.Math.Vector3 as Vector

import Control.Applicative ((<*>))
import Data.List
import Data.Functor
import Data.Monoid
import qualified Data.Foldable as F


-- | Represents an Octree.
data Octree e
    = Octree
    {
        root    :: AABB,
        ents    :: [e],
        c1      :: Octree e,
        c2      :: Octree e,
        c3      :: Octree e,
        c4      :: Octree e,
        c5      :: Octree e,
        c6      :: Octree e,
        c7      :: Octree e,
        c8      :: Octree e
    }
    |
    Leaf
    {
        root :: AABB,
        ents :: [e]
    }


-- | Builds an Octree using the specified AABB as the root and having the specified depth.
makeOctree :: Int -> AABB -> Octree e
makeOctree d root@(AABB min max)
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


subdivide :: AABB -> [AABB]
subdivide (AABB min max) = [a1, a2, a3, a4, a5, a6, a7, a8]
    where
        v = (max-min) / 2
        c = vec3 (x min + x v) (y min + y v) (z min + z v)
        a1 = AABB min c
        a2 = AABB ( vec3 (x min) (y min) (z c)   ) ( vec3 (x c)   (y c)   (z max) )
        a3 = AABB ( vec3 (x min) (y c)   (z min) ) ( vec3 (x c)   (y max) (z c)   )
        a4 = AABB ( vec3 (x min) (y c)   (z c)   ) ( vec3 (x c)   (y max) (z max) )
        a5 = AABB ( vec3 (x c)   (y min) (z min) ) ( vec3 (x max) (y c)   (z c)   )
        a6 = AABB ( vec3 (x c)   (y min) (z c)   ) ( vec3 (x max) (y c)   (z max) )
        a7 = AABB ( vec3 (x c)   (y c)   (z min) ) ( vec3 (x max) (y max) (z c)   )
        a8 = AABB c max


-- | Clones the structure of an octree. The new octree has no entities.
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
        
        
keep :: (e -> AABB -> CollisionType) -> AABB -> e -> Bool
keep testAABB aabb e = test == FullyContainedBy || test == Equal
    where test = e `testAABB` aabb


-- | Inserts an entity into the given octree.
insert :: (e -> AABB -> CollisionType) -> Octree e -> e -> Octree e
insert testAABB octree e = octree' where (octree', _) = insert' testAABB e octree


insert' :: (e -> AABB -> CollisionType) -> e -> Octree e -> (Octree e, Bool)


insert' testAABB e l@(Leaf root ents)
    | test == True = (Leaf root (e:ents), True)
    | otherwise    = (l, False)
    where
        test  = keep testAABB root e


insert' testAABB e o@(Octree root ents c1 c2 c3 c4 c5 c6 c7 c8)
    | test == False = (o, False)
    | otherwise =
        if isContainedInChild then (Octree root ents c1' c2' c3' c4' c5' c6' c7' c8', True)
        else (Octree root (e:ents) c1 c2 c3 c4 c5 c6 c7 c8, True)
        where
            children = [c1,c2,c3,c4,c5,c6,c7,c8]
            test    = keep testAABB root e
            descend = fmap (Spear.Math.Octree.insert' testAABB e) children
            (children', results) = unzip descend
            isContainedInChild = or results
            c1' = children' !! 0
            c2' = children' !! 1
            c3' = children' !! 2
            c4' = children' !! 3
            c5' = children' !! 4
            c6' = children' !! 5
            c7' = children' !! 6
            c8' = children' !! 7
            
            
-- | Inserts a list of entities into the given octree.
insertl :: (e -> AABB -> CollisionType) -> Octree e -> [e] -> Octree e
insertl testAABB octree es = octree' where (octree', _) = insertl' testAABB es octree


insertl' :: (e -> AABB -> CollisionType) -> [e] -> Octree e -> (Octree e, [e])

insertl' testAABB es (Leaf root ents) = (Leaf root ents', outliers)
    where
        ents'       = ents ++ ents_kept
        ents_kept   = filter (keep testAABB root) es
        outliers    = filter (not . keep testAABB root) es
        
insertl' testAABB es (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (Octree root ents' c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'        = ents ++ ents_kept
        new_ents     = es ++ ents1 ++ ents2 ++ ents3 ++ ents4 ++ ents5 ++ ents6 ++ ents7 ++ ents8
        ents_kept    = filter (keep testAABB root) new_ents
        outliers     = filter (not . keep testAABB root) new_ents
        (c1', ents1) = insertl' testAABB es c1
        (c2', ents2) = insertl' testAABB es c2
        (c3', ents3) = insertl' testAABB es c3
        (c4', ents4) = insertl' testAABB es c4
        (c5', ents5) = insertl' testAABB es c5
        (c6', ents6) = insertl' testAABB es c6
        (c7', ents7) = insertl' testAABB es c7
        (c8', ents8) = insertl' testAABB es c8


-- | Extracts all entities from an octree. The resulting octree has no entities.
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


-- | Applies the given function to the entities in the octree.
-- Entities that break out of their cell are reallocated appropiately.
map :: (e -> AABB -> CollisionType) -> (e -> e) -> Octree e -> Octree e
map testAABB f o = let (o', outliers) = map' testAABB f o in insertl testAABB o' outliers


map' :: (e -> AABB -> CollisionType) -> (e -> e) -> Octree e -> (Octree e, [e])


map' testAABB f (Leaf root ents) = (Leaf root ents_kept, outliers)
    where
        ents'       = fmap f ents
        ents_kept   = filter (keep testAABB root) ents'
        outliers    = filter (not . keep testAABB root) ents'


map' testAABB f (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (Octree root ents_kept c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'       = (fmap f ents) ++ out1 ++ out2 ++ out3 ++ out4 ++ out5 ++ out6 ++ out7 ++ out8
        ents_kept   = filter (keep testAABB root) ents'
        outliers    = filter (not . keep testAABB root) ents'
        (c1', out1) = map' testAABB f c1
        (c2', out2) = map' testAABB f c2
        (c3', out3) = map' testAABB f c3
        (c4', out4) = map' testAABB f c4
        (c5', out5) = map' testAABB f c5
        (c6', out6) = map' testAABB f c6
        (c7', out7) = map' testAABB f c7
        (c8', out8) = map' testAABB f c8


-- | Applies a function to the entity groups in the octree.
-- Entities that break out of their cell are reallocated appropiately.
gmap :: (e -> AABB -> CollisionType) -> (e -> e -> e) -> Octree e -> Octree e
gmap testAABB f o = let (o', outliers) = gmap' testAABB f o in insertl testAABB o' outliers


gmap' :: (e -> AABB -> CollisionType) -> (e -> e -> e) -> Octree e -> (Octree e, [e])

gmap' testAABB f (Leaf root ents) = (Leaf root ents_kept, outliers)
    where
        ents'       = f <$> ents <*> ents
        ents_kept   = filter (keep testAABB root) ents'
        outliers    = filter (not . keep testAABB root) ents'

gmap' testAABB f (Octree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (Octree root ents_kept c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'       = (f <$> ents <*> ents) ++ out1 ++ out2 ++ out3 ++ out4 ++ out5 ++ out6 ++ out7 ++ out8
        ents_kept   = filter (keep testAABB root) ents'
        outliers    = filter (not . keep testAABB root) ents'
        (c1', out1) = gmap' testAABB f c1
        (c2', out2) = gmap' testAABB f c2
        (c3', out3) = gmap' testAABB f c3
        (c4', out4) = gmap' testAABB f c4
        (c5', out5) = gmap' testAABB f c5
        (c6', out6) = gmap' testAABB f c6
        (c7', out7) = gmap' testAABB f c7
        (c8', out8) = gmap' testAABB f c8


population :: Octree e -> Int
population = F.foldr (\_ acc -> acc+1) 0




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
