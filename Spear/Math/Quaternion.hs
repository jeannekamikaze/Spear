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
,   Spear.Math.Quaternion.normalise
,   Spear.Math.Quaternion.norm
,   qrot
)
where


import qualified Spear.Math.Vector3 as V3
import Spear.Math.Vector4 as V4


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
qvec3 :: V3.Vector3 -> Float -> Quaternion
qvec3 v w = Quaternion $ vec4 (V3.x v) (V3.y v) (V3.z v) w


-- | Build a 'Quaternion' representing the given rotation.
qAxisAngle :: V3.Vector3 -> Float -> Quaternion
qAxisAngle axis angle =
    let s' = V3.norm axis
        s  = if s' == 0 then 1 else s'
        a  = angle * toRAD * 0.5
        sa = sin a
        w  = cos a
        x  = V3.x axis * sa * s
        y  = V3.y axis * sa * s
        z  = V3.z axis * sa * s
    in
        Quaternion $ vec4 x y z w


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
normalise :: Quaternion -> Quaternion
normalise = Quaternion . V4.normalise . getVec


-- | Compute the norm of the given 'Quaternion'.
norm :: Quaternion -> Float
norm = V4.norm . getVec


-- | Rotate the given 'Vector3'.
qrot :: Quaternion -> V3.Vector3 -> V3.Vector3
qrot q v = toVec3 $ q `qmul` qvec3 v 0 `qmul` qconj q
    where toVec3 (Quaternion q) = V3.vec3 (x q) (y q) (z q)


toRAD = pi / 180

