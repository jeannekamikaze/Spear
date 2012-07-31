module Spear.Math.MatrixUtils
(
    fastNormalMatrix
)
where


import Spear.Math.Matrix3 as M3
import Spear.Math.Matrix4 as M4


fastNormalMatrix :: Matrix4 -> Matrix3
fastNormalMatrix m =
    let m' = M4.transpose . M4.inverseTransform $ m
    in  M3.mat3
        (M4.m00 m') (M4.m10 m') (M4.m20 m')
        (M4.m01 m') (M4.m11 m') (M4.m21 m')
        (M4.m02 m') (M4.m12 m') (M4.m22 m')
