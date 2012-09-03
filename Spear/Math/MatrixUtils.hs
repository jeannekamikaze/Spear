module Spear.Math.MatrixUtils
(
    fastNormalMatrix
,   rpgTransform
,   pltTransform
,   rpgInverse
,   pltInverse
,   objToClip
)
where


import Spear.Math.Camera as Cam
import Spear.Math.Matrix3 as M3
import Spear.Math.Matrix4 as M4
import Spear.Math.Vector2 as V2
import Spear.Math.Vector3 as V3


-- | Compute the normal matrix of the given matrix.
fastNormalMatrix :: Matrix4 -> Matrix3
fastNormalMatrix m =
    let m' = M4.transpose . M4.inverseTransform $ m
    in  M3.mat3
        (M4.m00 m') (M4.m10 m') (M4.m20 m')
        (M4.m01 m') (M4.m11 m') (M4.m21 m')
        (M4.m02 m') (M4.m12 m') (M4.m22 m')


-- | Map an object's transform in view space to world space.
rpgTransform
    :: Float   -- ^ The height above the ground
    -> Float   -- ^ Angle of rotation
    -> Vector3 -- ^ Axis of rotation
    -> Vector2 -- ^ Object's position.
    -> Matrix4 -- ^ Inverse view matrix.
    -> Matrix4
rpgTransform h a axis pos viewInverse =
    let mat' = axisAngle axis a
        r = M4.right mat'
        u = M4.up mat'
        f = M4.forward mat'
        t = vec3 0 h 0 + vec3 (V2.x pos) 0 (-V2.y pos)
    in mat4
         (V3.x r) (V3.x u) (V3.x f) (V3.x t)
         (V3.y r) (V3.y u) (V3.y f) (V3.y t)
         (V3.z r) (V3.z u) (V3.z f) (V3.z t)
         0        0        0        1


-- | Map an object's transform in view space to world space.
pltTransform :: Matrix3 -> Matrix4
pltTransform mat =
    let r = let r' = M3.right mat in vec3 (V2.x r') (V2.y r') 0
        u = let u' = M3.up mat in vec3 (V2.x u') (V2.y u') 0
        f = V3.unitz
        t = let t' = M3.position mat in vec3 (V2.x t') (V2.y t') 0
    in mat4
        (V3.x r) (V3.x u) (V3.x f) (V3.x t)
        (V3.y r) (V3.y u) (V3.y f) (V3.y t)
        (V3.z r) (V3.z u) (V3.z f) (V3.z t)
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
    -> Vector2 -- ^ Object's position.
    -> Matrix4 -- ^ Inverse view matrix.
    -> Matrix4
rpgInverse h a rot pos viewInv = M4.inverseTransform $ rpgTransform h a rot pos viewInv


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
        view = M4.inverseTransform $ Cam.transform cam
        proj = Cam.projection cam
        p' = (proj * view * model) `M4.mulp` p
    in
        vec2 (V3.x p') (V3.y p')
