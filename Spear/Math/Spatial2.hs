module Spear.Math.Spatial2
(
    Spatial2(..)
,   Obj2
,   Angle
,   Radius
,   move
,   moveFwd
,   moveBack
,   moveUp
,   moveDown
,   moveLeft
,   moveRight
,   rotate
,   setRotation
,   pos
,   fwd
,   up
,   right
,   transform
,   setTransform
,   setPos
,   lookAt
,   Spear.Math.Spatial2.orbit
,   obj2FromVectors
,   obj2FromTransform
)
where

import Spear.Math.Vector
import qualified Spear.Math.Matrix3 as M

type Angle = Float
type Radius = Float

-- | An entity that can be moved around in 2D space.
class Spatial2 s where

      -- | Gets the spatial's Obj2.
      getObj2 :: s -> Obj2

      -- | Set the spatial's Obj2.
      setObj2 :: s -> Obj2 -> s

-- | Move the spatial.
move :: Spatial2 s => Vector2 -> s -> s
move v s = let o = getObj2 s in setObj2 s $ o { p = p o + v }

-- | Move the spatial forwards.
moveFwd :: Spatial2 s => Float -> s -> s
moveFwd a s = let o = getObj2 s in setObj2 s $ o { p = p o + scale a (fwd o) }

-- | Move the spatial backwards.
moveBack :: Spatial2 s => Float -> s -> s
moveBack a s = let o = getObj2 s in setObj2 s $ o { p = p o + scale (-a) (fwd o) }

-- | Move the spatial up.
moveUp :: Spatial2 s => Float -> s -> s
moveUp a s = let o = getObj2 s in setObj2 s $ o { p = p o + scale a (fwd o) }

-- | Move the spatial down.
moveDown :: Spatial2 s => Float -> s -> s
moveDown a s = let o = getObj2 s in setObj2 s $ o { p = p o + scale (-a) (fwd o) }

-- | Make the spatial strafe left.
moveLeft :: Spatial2 s => Float -> s -> s
moveLeft a s = let o = getObj2 s in setObj2 s $ o { p = p o + scale (-a) (right o) }

-- | Make the spatial Strafe right.
moveRight :: Spatial2 s => Float -> s -> s
moveRight a s = let o = getObj2 s in setObj2 s $ o { p = p o + scale a (right o) }

-- | Rotate the spatial.
rotate :: Spatial2 s => Float -> s -> s
rotate angle s = let o = getObj2 s in setObj2 s $ o
       { r = rotate' angle (r o)
       , u = rotate' angle (u o)
       }

-- | Set the spatial's rotation.
setRotation :: Spatial2 s => Float -> s -> s
setRotation angle s = let o = getObj2 s in setObj2 s $ o
            { r = rotate' angle unitx2
            , u = rotate' angle unity2
            }

rotate' :: Float -> Vector2 -> Vector2
rotate' a' (Vector2 x y) = vec2 (x * cos a) (y * sin a) where a = a'*pi/180

-- | Get the spatial's position.
pos :: Spatial2 s => s -> Vector2
pos = p . getObj2

-- | Get the spatial's forward vector.
fwd :: Spatial2 s => s -> Vector2
fwd = u . getObj2

-- | Get the spatial's up vector.
up :: Spatial2 s => s -> Vector2
up = u . getObj2

-- | Get the spatial's right vector.
right :: Spatial2 s => s -> Vector2
right = r . getObj2

-- | Get the spatial's transform.
transform :: Spatial2 s => s -> M.Matrix3
transform s = let o = getObj2 s in M.transform (r o) (u o) (p o)

-- | Set the spatial's transform.
setTransform :: Spatial2 s => M.Matrix3 -> s -> s
setTransform t s =
             let o = Obj2 (M.right t) (M.up t) (M.position t)
             in setObj2 s o

-- | Set the spatial's position.
setPos :: Spatial2 s => Vector2 -> s -> s
setPos pos s = setObj2 s $ (getObj2 s) { p = pos }

-- | Make the spatial look at the given point.
lookAt :: Spatial2 s => Vector2 -> s -> s
lookAt pt s =
       let position = pos s
           fwd      = normalise $ pt - position
           r        = perp fwd
       in setTransform (M.transform r fwd position) s

-- | Make the 'Spatial' orbit around the given point
orbit :: Spatial2 s => Vector2 -> Angle -> Radius -> s -> s
orbit pt angle radius s =
      let a = angle * pi / 180
          px = (x pt) + radius * sin a
          py = (y pt) + radius * cos a
      in setPos (vec2 px py) s

-- | An object in 2D space.
data Obj2 = Obj2
     { r :: Vector2
     , u :: Vector2
     , p :: Vector2
     } deriving Show

instance Spatial2 Obj2 where
         getObj2 = id
         setObj2 _ o' = o'

obj2FromVectors :: Right2 -> Up2 -> Position2 -> Obj2
obj2FromVectors = Obj2

obj2FromTransform :: M.Matrix3 -> Obj2
obj2FromTransform m = Obj2 (M.right m) (M.up m) (M.position m)