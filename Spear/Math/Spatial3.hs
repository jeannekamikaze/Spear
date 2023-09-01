{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Spear.Math.Spatial3
where

import           Spear.Math.Algebra
import qualified Spear.Math.Matrix4 as Matrix4
import           Spear.Math.Matrix4 (Matrix4)
import           Spear.Math.Spatial
import           Spear.Math.Vector
import           Spear.Prelude


data Rotation3
    = Pitch Angle
    | Yaw   Angle
    | Roll  Angle
    | AxisAngle Vector3 Angle
    | RotationMatrix Matrix4


-- | A 3D transform.
newtype Transform3 = Transform3 { transform3Matrix :: Matrix4 } deriving Show


type Positional3 a = Positional a Vector3
type Rotational3 a = Rotational a Angle
type Spatial3    s = Spatial    s Vector3 Rotation3 Transform3


instance Positional Transform3 Vector3 where
    setPosition p (Transform3 matrix) =
        Transform3 . Matrix4.setPosition p $ matrix

    position = Matrix4.position . transform3Matrix

    translate v t@(Transform3 matrix) = setPosition (Matrix4.position matrix + v) t


instance Rotational Transform3 Vector3 Rotation3 where
    setRotation rotation _ = Transform3 $ case rotation of
        Pitch     angle       -> Matrix4.rotX angle
        Yaw       angle       -> Matrix4.rotY angle
        Roll      angle       -> Matrix4.rotZ angle
        AxisAngle axis angle  -> Matrix4.axisAngle axis angle
        RotationMatrix matrix -> matrix

    rotation (Transform3 matrix) = RotationMatrix $ Matrix4.rotation matrix

    rotate rotation t@(Transform3 matrix) = case rotation of
        Pitch angle          -> pitch angle t
        Yaw angle            -> yaw   angle t
        Roll angle           -> roll  angle t
        AxisAngle axis angle -> Transform3 $ Matrix4.axisAngle axis angle * matrix
        RotationMatrix rot   -> Transform3 $ rot * matrix

    right (Transform3 matrix) = Matrix4.right matrix

    up (Transform3 matrix) = Matrix4.up matrix

    forward (Transform3 matrix )= Matrix4.forward matrix

    setForward forward (Transform3 matrix) =
        let right = forward `cross` unity3
            up    = right `cross` forward
       in Transform3 $ Matrix4.transform right up (neg forward) (Matrix4.position matrix)


instance Spatial Transform3 Vector3 Rotation3 Matrix4 where
    setTransform matrix _ = Transform3 $ Matrix4.transform
        (Matrix4.right matrix)
        (Matrix4.up matrix)
        (neg $ Matrix4.forward matrix)
        (Matrix4.position matrix)

    transform (Transform3 matrix) = Matrix4.transform
        (Matrix4.right matrix)
        (Matrix4.up matrix)
        (neg $ Matrix4.forward matrix)
        (Matrix4.position matrix)


class Has3dTransform a where
    -- | Set the object's 3d transform.
    set3dTransform :: Transform3 -> a -> a

    -- | Get the object's 3d transform.
    transform3 :: a -> Transform3


with3dTransform :: Has3dTransform a => (Transform3 -> Transform3) -> a -> a
with3dTransform f obj = set3dTransform (f $ transform3 obj) obj

-- | Build a 3d transform from right, up, forward and position vectors.
newTransform3 :: Vector3 -> Vector3 -> Vector3 -> Vector3 -> Transform3
newTransform3 right up forward pos = Transform3 $
    Matrix4.transform right up (neg forward) pos

-- | Rotate the object about the given axis.
rotate3 :: Vector3 -> Float -> Transform3 -> Transform3
rotate3 axis angle (Transform3 matrix) =
       let axis' = Matrix4.inverseTransform matrix `Matrix4.muld` axis
       in Transform3 $ matrix * Matrix4.axisAngle axis' angle

-- | Rotate the object about its local X axis.
pitch :: Float -> Transform3 -> Transform3
pitch angle (Transform3 matrix) =
    let sa = sin angle
        ca = cos angle
        f' = normalise $ (ca * Matrix4.forward matrix) + (sa * Matrix4.up matrix)
        u' = normalise $ Matrix4.right matrix `cross` f'
    in Transform3 . Matrix4.setUp u' . Matrix4.setForward f' $ matrix

-- | Rotate the object about its local Y axis.
yaw :: Float -> Transform3 -> Transform3
yaw angle (Transform3 matrix) =
    let sa = sin angle
        ca = cos angle
        r' = normalise $ (ca * Matrix4.right matrix) + (sa * Matrix4.forward matrix)
        f' = normalise $ Matrix4.up matrix `cross` r'
    in Transform3 . Matrix4.setRight r' . Matrix4.setForward f' $ matrix

-- | Rotate the object about its local Z axis.
roll :: Float -> Transform3 -> Transform3
roll angle (Transform3 matrix) =
    let sa = sin angle
        ca = cos angle
        u' = normalise $ (ca * Matrix4.up matrix) - (sa * Matrix4.right matrix)
        r' = normalise $ Matrix4.forward matrix `cross` u'
    in Transform3 . Matrix4.setRight r' . Matrix4.setUp u' $ matrix


-- | Make the object orbit around the given point
orbit :: Positional a Vector3
      => Vector3 -- ^ Target point
      -> Float   -- ^ Horizontal angle
      -> Float   -- ^ Vertical angle
      -> Float   -- ^ Orbit radius.
      -> a
      -> a
orbit pt anglex angley radius =
    let sx = sin anglex
        sy = sin angley
        cx = cos anglex
        cy = cos angley
        px = x pt + radius*cy*sx
        py = y pt + radius*sy
        pz = z pt + radius*cx*cy
    in setPosition (vec3 px py pz)
