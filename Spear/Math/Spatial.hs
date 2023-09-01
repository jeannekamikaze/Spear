{- This module categorizes objects in space. We identify three types of objects:

- Objects that only move (Positional).
- Objects that only rotate (Rotational).
- Objects that both move and rotate (Spatial).

Objects that only move are basically the rotationally-invariant ones: AABB,
circle, sphere, point light, omnidirectional sound source, etc.

Conversely for objects that only rotate, which are position-invariant:
directional light sources, for example, or a single vector.

Objects that both move and rotate are called "spatials". These are the
first-class citizens of space.

The lack of ad-hoc overloading in Haskell also makes function names a bit
annoying, so all the type classes here are general over 2d/3d space so that
we can use the same names for everything (e.g., "translate" to move an object,
regardless of whether it is a 2D or 3D object).
-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Spear.Math.Spatial where

import           Spear.Math.Algebra
import           Spear.Math.Vector
import           Spear.Prelude


type Angle  = Float -- TODO: consider newtype for Angle and Radius.
type Radius = Float -- TODO: Move somewhere more appropriate.

-- TODO: consider a general concept of Rotation (Angle and Quaternion) that
-- then conditions Rotational like Vector conditions Positional. That would
-- allow us to get a basis out of a Rotational much like we can do now with
-- Positional (because we know it operates on Vectors).


class Vector v => Positional a v | a -> v where
    -- | Set the object's position.
    setPosition :: v -> a -> a

    -- | Get the object's position.
    position :: a -> v

    -- | Translate the object.
    translate :: v -> a -> a


class Rotational a v r | a -> v, a -> r where
    -- | Set the object's rotation.
    setRotation :: r -> a -> a

    -- | Get the object's rotation.
    rotation :: a -> r

    -- | Rotate the object.
    rotate :: r -> a -> a

    -- | Get the object's right vector.
    right :: a -> v

    -- | Get the object's up vector.
    up :: a -> v

    -- | Get the object's forward vector.
    forward :: a -> v

    -- | Set the object's forward vector.
    setForward :: v -> a -> a


class (Positional a v, Rotational a v r) => Spatial a v r t | a -> t where
    -- | Set the spatial's transform.
    setTransform :: t -> a -> a

    -- | Get the spatial's transform.
    transform :: a -> t


--------------------------------------------------------------------------------
-- Spatial.

-- | Move the spatial along the given axis scaled by the given delta.
move :: Positional a v => Float -> (a -> v) -> a -> a
move delta axis a = translate (axis a * delta) a

-- | Move the spatial upwards.
moveRight delta = move delta right

-- | Move the spatial downwards.
moveLeft delta = moveRight (-delta)

-- | Move the spatial upwards.
moveUp delta = move delta up

-- | Move the spatial downwards.
moveDown delta = moveUp (-delta)

-- | Move the spatial forwards.
moveFwd delta = move delta forward

-- | Move the spatial backwards.
moveBack delta = moveFwd (-delta)

-- | Make the spatial look at the given point.
lookAt :: Vector v => Spatial a v r t => v -> a -> a
lookAt p a = setForward (normalise $ p - position a) a
