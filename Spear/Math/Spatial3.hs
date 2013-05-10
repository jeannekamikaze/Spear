module Spear.Math.Spatial3
(
    Spatial3(..)
,   Obj3
,   fromVectors
,   fromTransform
)
where

import Spear.Math.Vector
import qualified Spear.Math.Matrix4 as M

type Matrix4 = M.Matrix4

class Spatial3 s where
    -- | Gets the spatial's internal Obj3.
    getObj3 :: s -> Obj3

    -- | Set the spatial's internal Obj3.
    setObj3 :: s -> Obj3 -> s

    -- | Move the spatial.
    move :: Vector3 -> s -> s
    move d s = let o = getObj3 s in setObj3 s $ o { p = p o + d }

    -- | Move the spatial forwards.
    moveFwd :: Float -> s -> s
    moveFwd a s = let o = getObj3 s in setObj3 s $ o { p = p o + scale a (f o) }

    -- | Move the spatial backwards.
    moveBack :: Float -> s -> s
    moveBack a s = let o = getObj3 s in setObj3 s $ o { p = p o + scale (-a) (f o) }

    -- | Make the spatial strafe left.
    strafeLeft :: Float -> s -> s
    strafeLeft a s = let o = getObj3 s in setObj3 s $ o { p = p o + scale (-a) (r o) }

    -- | Make the spatial Strafe right.
    strafeRight :: Float -> s -> s
    strafeRight a s = let o = getObj3 s in setObj3 s $ o { p = p o + scale a (r o) }

    -- | Rotate the spatial about the given axis.
    rotate :: Vector3 -> Float -> s -> s
    rotate axis a s =
           let t = transform s
               axis' = M.inverseTransform t `M.muld` axis
           in setTransform (t * M.axisAngle axis' a) s

    -- | Rotate the spatial about its local X axis.
    pitch :: Float -> s -> s
    pitch a s =
          let o  = getObj3 s
              a' = toRAD a
              sa = sin a'
              ca = cos a'
              f' = normalise $ scale ca (f o) + scale sa (u o)
              u' = normalise $ r o `cross` f'
          in  setObj3 s $ o { u = u', f = f' }

    -- | Rotate the spatial about its local Y axis.
    yaw :: Float -> s -> s
    yaw a s =
        let o  = getObj3 s
            a' = toRAD a
            sa = sin a'
            ca = cos a'
            r' = normalise $ scale ca (r o) + scale sa (f o)
            f' = normalise $ u o `cross` r'
        in  setObj3 s $ o { r = r', f = f' }

    -- | Rotate the spatial about its local Z axis.
    roll :: Float -> s -> s
    roll a s =
         let o  = getObj3 s
             a' = toRAD a
             sa = sin a'
             ca = cos a'
             u' = normalise $ scale ca (u o) - scale sa (r o)
             r' = normalise $ f o `cross` u'
         in  setObj3 s $ o { r = r', u = u' }

    -- | Get the spatial's position.
    pos :: s -> Vector3
    pos = p . getObj3

    -- | Get the spatial's forward vector.
    fwd :: s -> Vector3
    fwd = f . getObj3

    -- | Get the spatial's up vector.
    up :: s -> Vector3
    up = u . getObj3

    -- | Get the spatial's right vector.
    right :: s -> Vector3
    right = r . getObj3

    -- | Get the spatial's transform.
    transform :: s -> Matrix4
    transform s = let o = getObj3 s in M.transform (r o) (u o) (scale (-1) $ f o) (p o)

    -- | Set the spatial's transform.
    setTransform :: Matrix4 -> s -> s
    setTransform t s =
                 let o = Obj3 (M.right t) (M.up t) (scale (-1) $ M.forward t) (M.position t)
                 in setObj3 s o

    -- | Set the spatial's position.
    setPos :: Vector3 -> s -> s
    setPos pos s = setObj3 s $ (getObj3 s) { p = pos }

    -- | Make the spatial look at the given point.
    lookAt :: Vector3 -> s -> s
    lookAt pt s =
        let position = pos s
            fwd      = normalise $ pt - position
            r        = fwd `cross` unity3
            u        = r `cross` fwd
        in
            setTransform (M.transform r u (-fwd) position) s

    -- | Make the spatial orbit around the given point
    orbit :: Vector3 -- ^ Target point
          -> Float   -- ^ Horizontal angle
          -> Float   -- ^ Vertical angle
          -> Float   -- ^ Orbit radius.
          -> s
          -> s

    orbit pt anglex angley radius s =
        let ax = anglex * pi / 180
            ay = angley * pi / 180
            sx = sin ax
            sy = sin ay
            cx = cos ax
            cy = cos ay
            px = (x pt) + radius*cy*sx
            py = (y pt) + radius*sy
            pz = (z pt) + radius*cx*cy
        in
            setPos (vec3 px py pz) s

-- | An object in 3D space.
data Obj3 = Obj3
     { r :: Vector3
     , u :: Vector3
     , f :: Vector3
     , p :: Vector3
     } deriving Show

instance Spatial3 Obj3 where
         getObj3 = id
         setObj3 _ o' = o'

fromVectors :: Right3 -> Up3 -> Forward3 -> Position3 -> Obj3
fromVectors = Obj3

fromTransform :: Matrix4 -> Obj3
fromTransform m = Obj3 (M.right m) (M.up m) (M.forward m) (M.position m)

toRAD = (*pi) . (/180)
