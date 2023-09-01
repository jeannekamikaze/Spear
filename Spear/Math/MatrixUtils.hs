{-# LANGUAGE NoImplicitPrelude #-}

module Spear.Math.MatrixUtils
(
    fastNormalMatrix
,   unproject
,   rpgUnproject
,   rpgTransform
,   pltTransform
,   rpgInverse
,   pltInverse
,   objToClip
)
where

import           Spear.Math.Camera   as Cam
import           Spear.Math.Matrix3  as M3
import           Spear.Math.Matrix4  as M4
import           Spear.Math.Spatial3 as S
import           Spear.Math.Vector   as V
import           Spear.Prelude

-- | Compute the normal matrix of the given matrix.
fastNormalMatrix :: Matrix4 -> Matrix3
fastNormalMatrix m =
    let m' = M4.transpose . M4.inverseTransform $ m
    in  M3.mat3
        (M4.m00 m') (M4.m10 m') (M4.m20 m')
        (M4.m01 m') (M4.m11 m') (M4.m21 m')
        (M4.m02 m') (M4.m12 m') (M4.m22 m')

-- | Transform the given point in window coordinates to object coordinates.
unproject :: Matrix4 -- ^ Inverse projection matrix
          -> Matrix4 -- ^ Inverse modelview matrix.
          -> Float   -- ^ Viewport x
          -> Float   -- ^ Viewport y
          -> Float   -- ^ Viewport width
          -> Float   -- ^ Viewport height
          -> Float   -- ^ Window x
          -> Float   -- ^ Window y
          -> Float   -- ^ Window z
          -> Vector3
unproject projI modelviewI vpx vpy w h x y z =
    let
        xmouse = (2::Float) * (x-vpx)/w - (1::Float)
        ymouse = (2::Float) * (y-vpy)/h - (1::Float)
        zmouse = (2::Float) * z - (1::Float)
    in
        (modelviewI * projI) `M4.mulp` vec3 xmouse ymouse zmouse

-- | Transform the given point in window coordinates to 2d coordinates.
--
-- The line defined by the given point in window space is intersected with
-- the XZ plane in world space to yield the resulting 2d point.
rpgUnproject
    :: Matrix4 -- ^ Inverse projection matrix
    -> Matrix4 -- ^ Inverse viewI matrix.
    -> Float   -- ^ Viewport x
    -> Float   -- ^ Viewport y
    -> Float   -- ^ Viewport width
    -> Float   -- ^ Viewport height
    -> Float   -- ^ Window x
    -> Float   -- ^ Window y
    -> Vector2
rpgUnproject projI viewI vpx vpy w h wx wy =
    let
        p1 = unproject projI viewI vpx vpy w h wx wy 0
        p2 = unproject projI viewI vpx vpy w h wx wy (-1)
        lambda = (y p1 / (y p1 - y p2))
        p' = p1 + lambda * (p2 - p1)
    in
        vec2 (x p') (-(z p'))

-- | Map an object's transform in view space to world space.
rpgTransform
    :: Float   -- ^ The height above the ground
    -> Float   -- ^ Angle of rotation
    -> Vector3 -- ^ Axis of rotation
    -> Vector2 -- ^ Object's position
    -> Matrix4 -- ^ Inverse view matrix
    -> Matrix4
rpgTransform h a axis pos viewI =
    let p1 = viewI `M4.mulp` vec3 (x pos) (y pos) 0
        p2 = viewI `M4.mulp` vec3 (x pos) (y pos) (-1)
        lambda  = (y p1 / (y p1 - y p2))
        p  = p1 + lambda * (p2 - p1)
        mat' = axisAngle axis a
        r = M4.right mat'
        u = M4.up mat'
        f = M4.forward mat'
        t = p + vec3 0 h 0
    in mat4
         (x r) (x u) (x f) (x t)
         (y r) (y u) (y f) (y t)
         (z r) (z u) (z f) (z t)
         0        0        0        1

-- | Map an object's transform in view space to world space.
pltTransform :: Matrix3 -> Matrix4
pltTransform mat =
    let r = let r' = M3.right mat in vec3 (x r') (y r') 0
        u = let u' = M3.up mat in vec3 (x u') (y u') 0
        f = unitz3
        t = let t' = M3.position mat in vec3 (x t') (y t') 0
    in mat4
        (x r) (x u) (x f) (x t)
        (y r) (y u) (y f) (y t)
        (z r) (z u) (z f) (z t)
        0        0        0        1

-- | Map an object's transform in world space to view space.
--
-- The XY plane in 2D translates to the X(-Z) plane in 3D.
--
-- Use this in games such as RPGs and RTSs.
rpgInverse
    :: Float   -- ^ The height above the ground
    -> Float   -- ^ Angle of rotation
    -> Vector3 -- ^ Axis of rotation
    -> Vector2 -- ^ Object's position
    -> Matrix4 -- ^ Inverse view matrix
    -> Matrix4
rpgInverse h a axis pos viewI =
    M4.inverseTransform $ rpgTransform h a axis pos viewI

-- | Map an object's transform in world space to view space.
--
-- This function maps an object's transform in 2D to the object's inverse in 3D.
--
-- The XY plane in 2D translates to the XY plane in 3D.
--
-- Use this in games like platformers and space invaders style games.
pltInverse :: Matrix3 -> Matrix4
pltInverse = M4.inverseTransform . pltTransform

-- | Transform an object from object to clip space coordinates.
objToClip :: Camera -> Matrix4 -> Vector3 -> Vector2
objToClip cam model p =
    let
        view = M4.inverseTransform . transform3Matrix . transform3 $ cam
        proj = projection cam
        p' = (proj * view * model) `M4.mulp` p
    in
        vec2 (x p') (y p')
