{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Spear.Math.Spatial2
where

import qualified Spear.Math.Matrix3 as Matrix3
import           Spear.Math.Matrix3 (Matrix3)
import           Spear.Math.Spatial as Spatial
import           Spear.Math.Vector
import           Spear.Prelude


type Positional2 a = Positional a Vector2
type Rotational2 a = Rotational a Angle
type Spatial2    s = Spatial    s Vector2 Angle Transform2


-- | A 2D transform.
newtype Transform2 = Transform2 { transform2Matrix :: Matrix3 } deriving Show


instance Rotational Vector2 Vector2 Angle where
    setRotation angle v = norm v * Vector2 (cos angle) (sin angle)

    rotation v@(Vector2 x _) = acos (x / norm v)

    rotate angle v = Vector2 (x v * cos angle) (y v * sin angle)

    right = perp

    up = id

    forward = id

    setForward newForward _ = newForward


instance Positional Transform2 Vector2 where
    setPosition p (Transform2 matrix) =
        Transform2 . Matrix3.setPosition p $ matrix

    position = Matrix3.position . transform2Matrix

    translate v t@(Transform2 matrix) = setPosition (Matrix3.position matrix + v) t


instance Rotational Transform2 Vector2 Angle where
    setRotation angle =
        Transform2 . Matrix3.setRight r' . Matrix3.setUp u' . transform2Matrix
        where r' = Spatial.rotate angle unitx2
              u' = Spatial.rotate angle unity2

    rotation = rotation . Matrix3.right . transform2Matrix

    rotate angle (Transform2 matrix) =
        Transform2 . Matrix3.setRight r' . Matrix3.setUp u' $ matrix
        where r' = Spatial.rotate angle (Matrix3.right matrix)
              u' = Spatial.rotate angle (Matrix3.up    matrix)

    right = Matrix3.right . transform2Matrix

    up = Matrix3.up . transform2Matrix

    forward = up

    setForward forward (Transform2 matrix) =
        Transform2 $ Matrix3.transform (perp forward) forward (Matrix3.position matrix)


instance Spatial Transform2 Vector2 Angle Matrix3 where
    setTransform matrix _ = Transform2 matrix

    transform (Transform2 matrix) = matrix


class Has2dTransform a where
    -- | Set the object's 2d transform.
    set2dTransform :: Transform2 -> a -> a

    -- | Get the object's 2d transform.
    transform2 :: a -> Transform2


with2dTransform :: Has2dTransform a => (Transform2 -> Transform2) -> a -> a
with2dTransform f obj = set2dTransform (f $ transform2 obj) obj

-- | Build a 2d transform from right, up, and position vectors.
newTransform2 :: Vector2 -> Vector2 -> Vector2 -> Transform2
newTransform2 right up position =
    Transform2 $ Matrix3.transform right up position

-- | Get a transform matrix from a 2d positional.
posTransform2 :: Positional a Vector2 => a -> Matrix3
posTransform2 = Matrix3.translatev . position

-- TODO: Get a transform matrix from a 2d rotational.

-- | Make the object orbit around the given point
--
-- This only changes the object's position and not its direction. Use 'lookAt'
-- to aim the object.
orbit :: Positional a Vector2 => Vector2 -> Angle -> Radius -> a -> a
orbit pt angle radius s =
    let px = x pt + radius * sin angle
        py = y pt + radius * cos angle
    in setPosition (vec2 px py) s
