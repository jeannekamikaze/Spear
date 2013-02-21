module Spear.Math.QuadTree
(
    QuadTree
,   makeQuadTree
,   clone
,   Spear.Math.QuadTree.insert
,   Spear.Math.QuadTree.map
,   gmap
)
where

import Spear.Collision
import Spear.Math.AABB
import Spear.Math.Vector

import Control.Applicative ((<*>))
import Data.List
import Data.Functor
import Data.Monoid
import qualified Data.Foldable as F


-- | Represents an QuadTree.
data QuadTree e
    = QuadTree
    { root :: !AABB
    , ents :: ![e]
    , c1   :: !(QuadTree e)
    , c2   :: !(QuadTree e)
    , c3   :: !(QuadTree e)
    , c4   :: !(QuadTree e)
    , c5   :: !(QuadTree e)
    , c6   :: !(QuadTree e)
    , c7   :: !(QuadTree e)
    , c8   :: !(QuadTree e)
    }
    |
    Leaf
    { root :: !AABB
    , ents :: ![e]
    }


-- | Builds an QuadTree using the specified AABB as the root and having the specified depth.
makeQuadTree :: Int -> AABB -> QuadTree e
makeQuadTree d root@(AABB min max)
    | d == 0    = Leaf root []
    | otherwise = QuadTree root [] c1 c2 c3 c4 c5 c6 c7 c8
    where
        boxes = subdivide root
        c1 = makeQuadTree (d-1) $ boxes !! 0
        c2 = makeQuadTree (d-1) $ boxes !! 1
        c3 = makeQuadTree (d-1) $ boxes !! 2
        c4 = makeQuadTree (d-1) $ boxes !! 3
        c5 = makeQuadTree (d-1) $ boxes !! 4
        c6 = makeQuadTree (d-1) $ boxes !! 5
        c7 = makeQuadTree (d-1) $ boxes !! 6
        c8 = makeQuadTree (d-1) $ boxes !! 7


subdivide :: AABB -> [AABB]
subdivide (AABB min max) = [a1, a2, a3, a4, a5, a6, a7, a8]
    where
        v = (max-min) / 2
        c = vec2 (x min + x v) (y min + y v)
        a1 = AABB min c
        a2 = AABB ( vec2 (x min) (y min)) ( vec2 (x c)   (y c)  )
        a3 = AABB ( vec2 (x min) (y c)  ) ( vec2 (x c)   (y max))
        a4 = AABB ( vec2 (x min) (y c)  ) ( vec2 (x c)   (y max))
        a5 = AABB ( vec2 (x c)   (y min)) ( vec2 (x max) (y c)  )
        a6 = AABB ( vec2 (x c)   (y min)) ( vec2 (x max) (y c)  )
        a7 = AABB ( vec2 (x c)   (y c)  ) ( vec2 (x max) (y max))
        a8 = AABB c max


-- | Clones the structure of an octree. The new octree has no entities.
clone :: QuadTree e -> QuadTree e
clone (Leaf root ents) = Leaf root []
clone (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) = QuadTree root [] c1' c2' c3' c4' c5' c6' c7' c8'
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
keep testAABB aabb e = test == FullyContainedBy
    where test = e `testAABB` aabb

  
-- | Inserts a list of entities into the given octree.
insert :: (e -> AABB -> CollisionType) -> QuadTree e -> [e] -> QuadTree e
insert testAABB octree es = octree' where (octree', _) = insert' testAABB es octree


insert' :: (e -> AABB -> CollisionType) -> [e] -> QuadTree e -> (QuadTree e, [e])

insert' testAABB es (Leaf root ents) = (Leaf root ents', outliers)
    where
        ents'       = ents ++ ents_kept
        ents_kept   = filter (keep testAABB root) es
        outliers    = filter (not . keep testAABB root) es
        
insert' testAABB es (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (QuadTree root ents' c1' c2' c3' c4' c5' c6' c7' c8', outliers)
    where
        ents'        = ents ++ ents_kept
        new_ents     = es ++ ents1 ++ ents2 ++ ents3 ++ ents4 ++ ents5 ++ ents6 ++ ents7 ++ ents8
        ents_kept    = filter (keep testAABB root) new_ents
        outliers     = filter (not . keep testAABB root) new_ents
        (c1', ents1) = insert' testAABB es c1
        (c2', ents2) = insert' testAABB es c2
        (c3', ents3) = insert' testAABB es c3
        (c4', ents4) = insert' testAABB es c4
        (c5', ents5) = insert' testAABB es c5
        (c6', ents6) = insert' testAABB es c6
        (c7', ents7) = insert' testAABB es c7
        (c8', ents8) = insert' testAABB es c8


-- | Extracts all entities from an octree. The resulting octree has no entities.
extract :: QuadTree e -> (QuadTree e, [e])
extract (Leaf root ents) = (Leaf root [], ents)
extract (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) = (QuadTree root [] c1' c2' c3' c4' c5' c6' c7' c8', ents')
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
-- Entities that break out of their cell are reallocated appropriately.
map :: (e -> AABB -> CollisionType) -> (e -> e) -> QuadTree e -> QuadTree e
map testAABB f o =
    let (o', outliers) = map' testAABB f o
    in Spear.Math.QuadTree.insert testAABB o' outliers


map' :: (e -> AABB -> CollisionType) -> (e -> e) -> QuadTree e -> (QuadTree e, [e])


map' testAABB f (Leaf root ents) = (Leaf root ents_kept, outliers)
    where
        ents'       = fmap f ents
        ents_kept   = filter (keep testAABB root) ents'
        outliers    = filter (not . keep testAABB root) ents'


map' testAABB f (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (QuadTree root ents_kept c1' c2' c3' c4' c5' c6' c7' c8', outliers)
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
-- Entities that break out of their cell are reallocated appropriately.
gmap :: (e -> AABB -> CollisionType) -> (e -> e -> e) -> QuadTree e -> QuadTree e
gmap testAABB f o =
    let (o', outliers) = gmap' testAABB f o
    in Spear.Math.QuadTree.insert testAABB o' outliers


gmap' :: (e -> AABB -> CollisionType) -> (e -> e -> e) -> QuadTree e -> (QuadTree e, [e])

gmap' testAABB f (Leaf root ents) = (Leaf root ents_kept, outliers)
    where
        ents'       = f <$> ents <*> ents
        ents_kept   = filter (keep testAABB root) ents'
        outliers    = filter (not . keep testAABB root) ents'

gmap' testAABB f (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
    (QuadTree root ents_kept c1' c2' c3' c4' c5' c6' c7' c8', outliers)
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


population :: QuadTree e -> Int
population = F.foldr (\_ acc -> acc+1) 0




instance Functor QuadTree where
    
    fmap f (Leaf root ents) = Leaf root $ fmap f ents
    
    fmap f (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
        QuadTree root (fmap f ents) c1' c2' c3' c4' c5' c6' c7' c8'
        where
            c1' = fmap f c1
            c2' = fmap f c2
            c3' = fmap f c3
            c4' = fmap f c4
            c5' = fmap f c5
            c6' = fmap f c6
            c7' = fmap f c7
            c8' = fmap f c8



instance F.Foldable QuadTree where
    
    foldMap f (Leaf root ents) = mconcat . fmap f $ ents
    
    foldMap f (QuadTree root ents c1 c2 c3 c4 c5 c6 c7 c8) =
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
