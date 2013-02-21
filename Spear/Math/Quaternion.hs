module Spear.Math.Quaternion
(
    Quaternion
    -- * Construction
,   quat
,   qvec4
,   qvec3
,   qAxisAngle
    -- * Operations
,   qmul
,   qconj
,   qinv
,   qnormalise
,   qnorm
,   qrot
)
where


import Spear.Math.Vector


newtype Quaternion = Quaternion { getVec :: Vector4 }


-- | Build a 'Quaternion'.
quat :: Float -- x
     -> Float -- y
     -> Float -- z
     -> Float -- w
     -> Quaternion
quat x y z w = Quaternion $ vec4 x y z w


-- | Build a 'Quaternion' from the given 'Vector4'.
qvec4 :: Vector4 -> Quaternion
qvec4 = Quaternion


-- | Build a 'Quaternion' from the given 'Vector3' and w.
qvec3 :: Vector3 -> Float -> Quaternion
qvec3 v w = Quaternion $ vec4 (x v) (y v) (z v) w


-- | Build a 'Quaternion' representing the given rotation.
qAxisAngle :: Vector3 -> Float -> Quaternion
qAxisAngle axis angle =
    let s'  = norm axis
        s   = if s' == 0 then 1 else s'
        a   = angle * toRAD * 0.5
        sa  = sin a
        qw  = cos a
        qx  = x axis * sa * s
        qy  = y axis * sa * s
        qz  = z axis * sa * s
    in
        Quaternion $ vec4 qx qy qz qw


-- | Compute the product of the given two quaternions.
qmul :: Quaternion -> Quaternion -> Quaternion
qmul (Quaternion q1) (Quaternion q2) =
    let x1 = x q1
        y1 = y q1
        z1 = z q1
        w1 = w q1
        x2 = x q2
        y2 = y q2
        z2 = y q2
        w2 = w q2
        w' = w1*w2 - x1*x2 - y1*y2 - z1*z2
        x' = w1*x2 + x1*w2 + y1*z2 - z1*y2
        y' = w1*y2 - x1*z2 + y1*w2 + z1*x2
        z' = w1*z2 + x1*y2 - y1*x2 + z1*w2
    in
        Quaternion $ vec4 x' y' z' w'


-- | Compute the conjugate of the given 'Quaternion'.
qconj :: Quaternion -> Quaternion
qconj (Quaternion q) = Quaternion $ vec4 (-x q) (-y q) (-z q) (w q)


-- | Invert the given 'Quaternion'.
qinv :: Quaternion -> Quaternion
qinv (Quaternion q) =
    let m = normSq q
    in Quaternion $ vec4 (-x q / m) (-y q / m) (-z q / m) (w q / m)


-- | Normalise the given 'Quaternion'.
qnormalise :: Quaternion -> Quaternion
qnormalise = Quaternion . normalise . getVec


-- | Compute the norm of the given 'Quaternion'.
qnorm :: Quaternion -> Float
qnorm = norm . getVec


-- | Rotate the given 'Vector3'.
qrot :: Quaternion -> Vector3 -> Vector3
qrot q v = toVec3 $ q `qmul` qvec3 v 0 `qmul` qconj q
    where toVec3 (Quaternion q) = vec3 (x q) (y q) (z q)


toRAD = pi / 180

