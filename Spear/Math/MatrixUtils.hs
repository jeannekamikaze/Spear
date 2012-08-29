module Spear.Math.MatrixUtils
(
    fastNormalMatrix
,   rpgTransform
,   pltTransform
,   rpgInverse
,   pltInverse
)
where


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


-- | Maps the given 2D transformation matrix to a 3D transformation matrix.
rpgTransform :: Float -- ^ The height above the ground.
             -> Matrix3 -> Matrix4
rpgTransform h mat =
    let r = let r' = M3.right mat in vec3 (V2.x r') (V2.y r') 0
        u = V3.unity
        f = let f' = M3.forward mat in vec3 (V2.x f') 0 (V2.y f')
        t = (vec3 0 h 0) + let t' = M3.position mat in -(vec3 (V2.x t') 0 (-V2.y t'))
    in mat4
        (V3.x r) (V3.x u) (V3.x f) (V3.x t)
        (V3.y r) (V3.y u) (V3.y f) (V3.y t)
        (V3.z r) (V3.z u) (V3.z f) (V3.z t)
        0        0        0        1


-- | Maps the given 2D transformation matrix to a 3D transformation matrix.
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


-- | Compute the inverse transform of the given transformation matrix.
--
-- This function maps an object's transform in 2D to the object's inverse in 3D.
-- 
-- The XY plane in 2D translates to the X(-Z) plane in 3D.
--
-- Use this in games such as RPGs and RTSs.
rpgInverse :: Float -- ^ Height above the ground.
           -> Matrix3 -> Matrix4
rpgInverse h mat =
    let r = let r' = M3.right mat in vec3 (V2.x r') (V2.y r') 0
        u = V3.unity
        f = let f' = M3.forward mat in vec3 (V2.x f') 0 (-V2.y f')
        t = (vec3 0 h 0) + let t' = M3.position mat in -(vec3 (V2.x t') 0 (-V2.y t'))
    in mat4
        (V3.x r) (V3.y r) (V3.z r) (t `V3.dot` r)
        (V3.x u) (V3.y u) (V3.z u) (t `V3.dot` u)
        (V3.x f) (V3.y f) (V3.z f) (t `V3.dot` f)
        0        0        0        1


-- | Compute the inverse transform of the given transformation matrix.
--
-- This function maps an object's transform in 2D to the object's inverse in 3D.
-- 
-- The XY plane in 2D translates to the XY plane in 3D.
-- 
-- Use this in games like platformers and space invaders style games.
pltInverse :: Matrix3 -> Matrix4
pltInverse mat =
    let r = let r' = M3.right mat in vec3 (V2.x r') (V2.y r') 0
        u = let u' = M3.up mat in vec3 (V2.x u') (V2.y u') 0
        f = V3.unitz
        t = let t' = M3.position mat in vec3 (V2.x t') (V2.y t') 0
    in mat4
        (V3.x r) (V3.y r) (V3.z r) (t `V3.dot` r)
        (V3.x u) (V3.y u) (V3.z u) (t `V3.dot` u)
        (V3.x f) (V3.y f) (V3.z f) (t `V3.dot` f)
        0        0        0        1
