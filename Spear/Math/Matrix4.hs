{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Spear.Math.Matrix4
(
    Matrix4
    -- * Accessors
,   m00, m01, m02, m03
,   m10, m11, m12, m13
,   m20, m21, m22, m23
,   m30, m31, m32, m33
,   col0, col1, col2, col3
,   row0, row1, row2, row3
,   right, up, forward, position
,   setRight, setUp, setForward, setPosition
    -- * Construction
,   mat4
,   mat4fromVec
,   transform
,   translation
,   rotation
,   lookAt
,   Spear.Math.Matrix4.id
    -- * Transformations
    -- ** Translation
,   transl
,   translv
    -- ** Rotation
,   rotX
,   rotY
,   rotZ
,   axisAngle
    -- ** Scale
,   Spear.Math.Matrix4.scale
,   scalev
    -- ** Reflection
,   reflectX
,   reflectY
,   reflectZ
    -- ** Projection
,   ortho
,   perspective
,   planeProj
    -- * Operations
,   Spear.Math.Matrix4.zipWith
,   Spear.Math.Matrix4.map
,   transpose
,   inverseTransform
,   inverse
,   mul
,   mulp
,   muld
,   mul'
)
where

import           Spear.Math.Algebra hiding (mul)
import           Spear.Math.Vector
import           Spear.Prelude      hiding (mul)

import           Foreign.Storable


-- | Represents a 4x4 column major matrix.
data Matrix4 = Matrix4
    { m00 :: {-# UNPACK #-} !Float, m10 :: {-# UNPACK #-} !Float, m20 :: {-# UNPACK #-} !Float, m30 :: {-# UNPACK #-} !Float
    , m01 :: {-# UNPACK #-} !Float, m11 :: {-# UNPACK #-} !Float, m21 :: {-# UNPACK #-} !Float, m31 :: {-# UNPACK #-} !Float
    , m02 :: {-# UNPACK #-} !Float, m12 :: {-# UNPACK #-} !Float, m22 :: {-# UNPACK #-} !Float, m32 :: {-# UNPACK #-} !Float
    , m03 :: {-# UNPACK #-} !Float, m13 :: {-# UNPACK #-} !Float, m23 :: {-# UNPACK #-} !Float, m33 :: {-# UNPACK #-} !Float
    }


instance Show Matrix4 where
    show (Matrix4 m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33) =
        show' m00 ++ ", " ++ show' m10 ++ ", " ++ show' m20 ++ ", " ++ show' m30 ++ "\n" ++
        show' m01 ++ ", " ++ show' m11 ++ ", " ++ show' m21 ++ ", " ++ show' m31 ++ "\n" ++
        show' m02 ++ ", " ++ show' m12 ++ ", " ++ show' m22 ++ ", " ++ show' m32 ++ "\n" ++
        show' m03 ++ ", " ++ show' m13 ++ ", " ++ show' m23 ++ ", " ++ show' m33 ++ "\n"
        where
            show' f = if abs f < 0.0000001 then "0" else show f


instance Addition Matrix4 Matrix4 where
    (Matrix4 a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15)
        + (Matrix4 b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15)
            = Matrix4 (a00 + b00) (a01 + b01) (a02 + b02) (a03 + b03)
                      (a04 + b04) (a05 + b05) (a06 + b06) (a07 + b07)
                      (a08 + b08) (a09 + b09) (a10 + b10) (a11 + b11)
                      (a12 + b12) (a13 + b13) (a14 + b14) (a15 + b15)


instance Subtraction Matrix4 Matrix4 where
    (Matrix4 a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15)
        - (Matrix4 b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15)
            = Matrix4 (a00 - b00) (a01 - b01) (a02 - b02) (a03 - b03)
                      (a04 - b04) (a05 - b05) (a06 - b06) (a07 - b07)
                      (a08 - b08) (a09 - b09) (a10 - b10) (a11 - b11)
                      (a12 - b12) (a13 - b13) (a14 - b14) (a15 - b15)


instance Product Matrix4 Matrix4 Matrix4 where
    (Matrix4 a00 a10 a20 a30 a01 a11 a21 a31 a02 a12 a22 a32 a03 a13 a23 a33)
        * (Matrix4 b00 b10 b20 b30 b01 b11 b21 b31 b02 b12 b22 b32 b03 b13 b23 b33)
            = Matrix4 (a00 * b00 + a10 * b01 + a20 * b02 + a30 * b03)
                      (a00 * b10 + a10 * b11 + a20 * b12 + a30 * b13)
                      (a00 * b20 + a10 * b21 + a20 * b22 + a30 * b23)
                      (a00 * b30 + a10 * b31 + a20 * b32 + a30 * b33)

                      (a01 * b00 + a11 * b01 + a21 * b02 + a31 * b03)
                      (a01 * b10 + a11 * b11 + a21 * b12 + a31 * b13)
                      (a01 * b20 + a11 * b21 + a21 * b22 + a31 * b23)
                      (a01 * b30 + a11 * b31 + a21 * b32 + a31 * b33)

                      (a02 * b00 + a12 * b01 + a22 * b02 + a32 * b03)
                      (a02 * b10 + a12 * b11 + a22 * b12 + a32 * b13)
                      (a02 * b20 + a12 * b21 + a22 * b22 + a32 * b23)
                      (a02 * b30 + a12 * b31 + a22 * b32 + a32 * b33)

                      (a03 * b00 + a13 * b01 + a23 * b02 + a33 * b03)
                      (a03 * b10 + a13 * b11 + a23 * b12 + a33 * b13)
                      (a03 * b20 + a13 * b21 + a23 * b22 + a33 * b23)
                      (a03 * b30 + a13 * b31 + a23 * b32 + a33 * b33)


instance Product Matrix4 Float Matrix4 where
    (Matrix4 a00 a10 a20 a30 a01 a11 a21 a31 a02 a12 a22 a32 a03 a13 a23 a33) * s =
        Matrix4 (a00 * s) (a10 * s) (a20 * s) (a30 * s)
                (a01 * s) (a11 * s) (a21 * s) (a31 * s)
                (a02 * s) (a12 * s) (a22 * s) (a32 * s)
                (a03 * s) (a13 * s) (a23 * s) (a33 * s)


instance Storable Matrix4 where
    sizeOf    _ = 64
    alignment _ = 4

    peek ptr = do
        a00 <- peekByteOff ptr 0;  a01 <- peekByteOff ptr 4;  a02 <- peekByteOff ptr 8;  a03 <- peekByteOff ptr 12;
        a10 <- peekByteOff ptr 16; a11 <- peekByteOff ptr 20; a12 <- peekByteOff ptr 24; a13 <- peekByteOff ptr 28;
        a20 <- peekByteOff ptr 32; a21 <- peekByteOff ptr 36; a22 <- peekByteOff ptr 40; a23 <- peekByteOff ptr 44;
        a30 <- peekByteOff ptr 48; a31 <- peekByteOff ptr 52; a32 <- peekByteOff ptr 56; a33 <- peekByteOff ptr 60;

        return $ Matrix4 a00 a10 a20 a30
                         a01 a11 a21 a31
                         a02 a12 a22 a32
                         a03 a13 a23 a33

    poke ptr (Matrix4 a00 a10 a20 a30
                      a01 a11 a21 a31
                      a02 a12 a22 a32
                      a03 a13 a23 a33) = do
        pokeByteOff ptr 0  a00; pokeByteOff ptr 4  a01; pokeByteOff ptr 8  a02; pokeByteOff ptr 12 a03;
        pokeByteOff ptr 16 a10; pokeByteOff ptr 20 a11; pokeByteOff ptr 24 a12; pokeByteOff ptr 28 a13;
        pokeByteOff ptr 32 a20; pokeByteOff ptr 36 a21; pokeByteOff ptr 40 a22; pokeByteOff ptr 44 a23;
        pokeByteOff ptr 48 a30; pokeByteOff ptr 52 a31; pokeByteOff ptr 56 a32; pokeByteOff ptr 60 a33;


col0 (Matrix4 a00 _   _   _   a01 _   _   _   a02 _   _   _   a03 _   _   _  ) = vec4 a00 a01 a02 a03
col1 (Matrix4 _   a10 _   _   _   a11 _   _   _   a12 _   _   _   a13 _   _  ) = vec4 a10 a11 a12 a13
col2 (Matrix4 _   _   a20 _   _   _   a21 _   _   _   a22 _   _   _   a23 _  ) = vec4 a20 a21 a22 a23
col3 (Matrix4 _   _   _   a30 _   _   _   a31 _   _   _   a32 _   _   _   a33) = vec4 a30 a31 a32 a33

row0 (Matrix4 a00 a01 a02 a03 _   _   _   _   _   _   _   _   _   _   _   _  ) = vec4 a00 a01 a02 a03
row1 (Matrix4 _   _   _   _   a10 a11 a12 a13 _   _   _   _   _   _   _   _  ) = vec4 a10 a11 a12 a13
row2 (Matrix4 _   _   _   _   _   _   _   _   a20 a21 a22 a23 _   _   _   _  ) = vec4 a20 a21 a22 a23
row3 (Matrix4 _   _   _   _   _   _   _   _   _   _   _   _   a30 a31 a32 a33) = vec4 a30 a31 a32 a33

right    (Matrix4 a00 _   _   _   a01 _   _   _   a02 _   _   _   _   _   _   _) = vec3 a00 a01 a02
up       (Matrix4 _   a10 _   _   _   a11 _   _   _   a12 _   _   _   _   _   _) = vec3 a10 a11 a12
forward  (Matrix4 _   _   a20 _   _   _   a21 _   _   _   a22 _   _   _   _   _) = vec3 a20 a21 a22
position (Matrix4 _   _   _   a30 _   _   _   a31 _   _   _   a32 _   _   _   _) = vec3 a30 a31 a32

setRight    (Vector3 x y z) matrix = matrix { m00 = x, m01 = y, m02 = z }
setUp       (Vector3 x y z) matrix = matrix { m10 = x, m11 = y, m12 = z }
setForward  (Vector3 x y z) matrix = matrix { m20 = x, m21 = y, m22 = z }
setPosition (Vector3 x y z) matrix = matrix { m30 = x, m31 = y, m32 = z }

-- | Build a matrix from the specified values.
mat4 = Matrix4

-- | Build a matrix from four vectors in 4D.
mat4fromVec :: Vector4 -> Vector4 -> Vector4 -> Vector4 -> Matrix4
mat4fromVec v0 v1 v2 v3 = Matrix4
    (x v0) (x v1) (x v2) (x v3)
    (y v0) (y v1) (y v2) (y v3)
    (z v0) (z v1) (z v2) (z v3)
    (w v0) (w v1) (w v2) (w v3)

-- | Build a transformation 'Matrix4' from the given vectors.
transform :: Vector3 -- ^ Right vector.
          -> Vector3 -- ^ Up vector.
          -> Vector3 -- ^ Forward vector.
          -> Vector3 -- ^ Position.
          -> Matrix4
transform right up fwd pos = mat4
    (x right) (x up) (x fwd) (x pos)
    (y right) (y up) (y fwd) (y pos)
    (z right) (z up) (z fwd) (z pos)
           0      0       0       1

-- | Get the translation part of the given transformation matrix.
translation :: Matrix4 -> Matrix4
translation (Matrix4
    a00 a10 a20 a30
    a01 a11 a21 a31
    a02 a12 a22 a32
    a03 a13 a23 a33)
    = mat4
    1   0   0   a30
    0   1   0   a31
    0   0   1   a32
    0   0   0   a33

-- | Get the rotation part of the given transformation matrix.
rotation :: Matrix4 -> Matrix4
rotation (Matrix4
    a00 a10 a20 a30
    a01 a11 a21 a31
    a02 a12 a22 a32
    a03 a13 a23 a33)
    = mat4
    a00 a10 a20 0
    a01 a11 a21 0
    a02 a12 a22 0
    a03 a13 a23 1

-- | Build a transformation 'Matrix4' defined by the given position and target.
lookAt :: Vector3 -- ^ Eye position.
       -> Vector3 -- ^ Target point.
       -> Matrix4
lookAt pos target =
        let fwd = normalise $ target - pos
            r    = fwd `cross` unity3
            u    = r `cross` fwd
        in
            transform r u (-fwd) pos

-- | Zip two matrices together with the specified function.
zipWith :: (Float -> Float -> Float) -> Matrix4 -> Matrix4 -> Matrix4
zipWith f a b = Matrix4
    (f (m00 a) (m00 b)) (f (m10 a) (m10 b)) (f (m20 a) (m20 b)) (f (m30 a) (m30 b))
    (f (m01 a) (m01 b)) (f (m11 a) (m11 b)) (f (m21 a) (m21 b)) (f (m31 a) (m31 b))
    (f (m02 a) (m02 b)) (f (m12 a) (m12 b)) (f (m22 a) (m22 b)) (f (m32 a) (m32 b))
    (f (m03 a) (m03 b)) (f (m13 a) (m13 b)) (f (m23 a) (m23 b)) (f (m33 a) (m33 b))

-- | Map the specified function to the specified matrix.
map :: (Float -> Float) -> Matrix4 -> Matrix4
map f m = Matrix4
    (f . m00 $ m) (f . m10 $ m) (f . m20 $ m) (f . m30 $ m)
    (f . m01 $ m) (f . m11 $ m) (f . m21 $ m) (f . m31 $ m)
    (f . m02 $ m) (f . m12 $ m) (f . m22 $ m) (f . m32 $ m)
    (f . m03 $ m) (f . m13 $ m) (f . m23 $ m) (f . m33 $ m)

-- | Return the identity matrix.
id :: Matrix4
id = mat4
    1   0   0   0
    0   1   0   0
    0   0   1   0
    0   0   0   1

-- | Create a translation matrix.
transl :: Float -> Float -> Float -> Matrix4
transl x y z = mat4
    1   0   0   x
    0   1   0   y
    0   0   1   z
    0   0   0   1

-- | Create a translation matrix.
translv :: Vector3 -> Matrix4
translv v = mat4
    1    0    0    (x v)
    0    1    0    (y v)
    0    0    1    (z v)
    0    0    0    1

-- | Create a rotation matrix rotating about the X axis.
-- The given angle must be in degrees.
rotX :: Float -> Matrix4
rotX angle = mat4
    1    0    0    0
    0    c    (-s) 0
    0    s    c    0
    0    0    0    1
    where
        s = sin angle
        c = cos angle

-- | Create a rotation matrix rotating about the Y axis.
-- The given angle must be in degrees.
rotY :: Float -> Matrix4
rotY angle = mat4
    c    0    s    0
    0    1    0    0
    (-s) 0    c    0
    0    0    0    1
    where
        s = sin angle
        c = cos angle

-- | Create a rotation matrix rotating about the Z axis.
-- The given angle must be in degrees.
rotZ :: Float -> Matrix4
rotZ angle = mat4
    c    (-s) 0    0
    s    c    0    0
    0    0    1    0
    0    0    0    1
    where
        s = sin angle
        c = cos angle

-- | Create a rotation matrix rotating about the specified axis.
-- The given angle must be in degrees.
axisAngle :: Vector3 -> Float -> Matrix4
axisAngle v angle = mat4
    (c + omc*ax^2) (omc*xy-sz)  (omc*xz+sy)  0
    (omc*xy+sz)    (c+omc*ay^2) (omc*yz-sx)  0
    (omc*xz-sy)    (omc*yz+sx)  (c+omc*az^2) 0
     0             0           0             1
    where
        ax  = x v
        ay  = y v
        az  = z v
        s   = sin angle
        c   = cos angle
        xy  = ax*ay
        xz  = ax*az
        yz  = ay*az
        sx  = s*ax
        sy  = s*ay
        sz  = s*az
        omc = (1::Float) - c

-- | Create a scale matrix.
scale :: Float -> Float -> Float -> Matrix4
scale sx sy sz = mat4
    sx  0   0   0
    0   sy  0   0
    0   0   sz  0
    0   0   0   1

-- | Create a scale matrix.
scalev :: Vector3 -> Matrix4
scalev v = mat4
    sx  0   0   0
    0   sy  0   0
    0   0   sz  0
    0   0   0   1
        where
            sx = x v
            sy = y v
            sz = z v

-- | Create an X reflection matrix.
reflectX :: Matrix4
reflectX = mat4
    (-1)  0   0   0
    0     1   0   0
    0     0   1   0
    0     0   0   1

-- | Create a Y reflection matrix.
reflectY :: Matrix4
reflectY = mat4
    1   0     0   0
    0   (-1)  0   0
    0   0     1   0
    0   0     0   1

-- | Create a Z reflection matrix.
reflectZ :: Matrix4
reflectZ = mat4
    1   0   0     0
    0   1   0     0
    0   0   (-1)  0
    0   0   0     1

-- | Create an orthogonal projection matrix.
ortho :: Float -- ^ Left.
      -> Float -- ^ Right.
      -> Float -- ^ Bottom.
      -> Float -- ^ Top.
      -> Float -- ^ Near clip.
      -> Float -- ^ Far clip.
      -> Matrix4
ortho l r b t n f =
    let tx = (-(r+l)/(r-l))
        ty = (-(t+b)/(t-b))
        tz = (-(f+n)/(f-n))
    in mat4
        (2/(r-l)) 0         0            tx
        0         (2/(t-b)) 0            ty
        0         0         ((-2)/(f-n)) tz
        0         0         0            1

-- | Create a perspective projection matrix.
perspective :: Float -- ^ Fovy - Vertical field of view angle in degrees.
            -> Float -- ^ Aspect ratio.
            -> Float -- ^ Near clip distance.
            -> Float -- ^ Far clip distance
            -> Matrix4
perspective fovy r near far =
    let f = 1 / tan (fovy / (2::Float))
        a = near - far
    in mat4
        (f/r) 0    0              0
        0     f    0              0
        0     0    ((far+near)/a) ((2::Float)*far*near/a)
        0     0    (-1)           0

-- | Create a plane projection matrix.
planeProj :: Vector3 -- ^ Plane normal
          -> Float   -- ^ Plane distance from the origin
          -> Vector3 -- ^ Projection direction
          -> Matrix4
planeProj n d l =
    let c = n `dot` l
        nx = x n
        ny = y n
        nz = z n
        lx = x l
        ly = y l
        lz = z l
    in mat4
        (d + c - nx*lx) (-ny*lx)        (-nz*lx)        (-lx*d)
        (-nx*ly)        (d + c - ny*ly) (-nz*ly)        (-ly*d)
        (-nx*lz)        (-ny*lz)        (d + c - nz*lz) (-lz*d)
        (-nx)           (-ny)           (-nz)           c

-- | Transpose the specified matrix.
transpose :: Matrix4 -> Matrix4
transpose m = mat4
    (m00 m) (m01 m) (m02 m) (m03 m)
    (m10 m) (m11 m) (m12 m) (m13 m)
    (m20 m) (m21 m) (m22 m) (m23 m)
    (m30 m) (m31 m) (m32 m) (m33 m)

-- | Invert the given transformation matrix.
inverseTransform :: Matrix4 -> Matrix4
inverseTransform mat =
    let
        r = right mat
        u = up mat
        f = forward mat
        t = position mat
    in
        mat4
            (x r) (y r) (z r) (-t `dot` r)
            (x u) (y u) (z u) (-t `dot` u)
            (x f) (y f) (z f) (-t `dot` f)
            0        0        0        1

-- | Invert the given matrix.
inverse :: Matrix4 -> Matrix4
inverse mat =
    let
        a00 = m00 mat
        a01 = m01 mat
        a02 = m02 mat
        a03 = m03 mat
        a04 = m10 mat
        a05 = m11 mat
        a06 = m12 mat
        a07 = m13 mat
        a08 = m20 mat
        a09 = m21 mat
        a10 = m22 mat
        a11 = m23 mat
        a12 = m30 mat
        a13 = m31 mat
        a14 = m32 mat
        a15 = m33 mat

        m00' = a05 * a10  * a15
             - a05 * a11  * a14
             - a09 * a06  * a15
             + a09 * a07  * a14
             + a13 * a06  * a11
             - a13 * a07  * a10

        m04' = -a04 * a10 * a15
             +  a04 * a11 * a14
             +  a08 * a06 * a15
             -  a08 * a07 * a14
             -  a12 * a06 * a11
             +  a12 * a07 * a10

        m08' = a04 * a09 * a15
             - a04 * a11 * a13
             - a08 * a05 * a15
             + a08 * a07 * a13
             + a12 * a05 * a11
             - a12 * a07 * a09

        m12' = -a04 * a09 * a14
             +  a04 * a10 * a13
             +  a08 * a05 * a14
             -  a08 * a06 * a13
             -  a12 * a05 * a10
             +  a12 * a06 * a09

        m01' = -a01 * a10 * a15
             +  a01 * a11 * a14
             +  a09 * a02 * a15
             -  a09 * a03 * a14
             -  a13 * a02 * a11
             +  a13 * a03 * a10

        m05' = a00 * a10 * a15
             - a00 * a11 * a14
             - a08 * a02 * a15
             + a08 * a03 * a14
             + a12 * a02 * a11
             - a12 * a03 * a10

        m09' = -a00 * a09 * a15
             +  a00 * a11 * a13
             +  a08 * a01 * a15
             -  a08 * a03 * a13
             -  a12 * a01 * a11
             +  a12 * a03 * a09

        m13' = a00 * a09 * a14
             - a00 * a10 * a13
             - a08 * a01 * a14
             + a08 * a02 * a13
             + a12 * a01 * a10
             - a12 * a02 * a09

        m02' = a01 * a06 * a15
             - a01 * a07 * a14
             - a05 * a02 * a15
             + a05 * a03 * a14
             + a13 * a02 * a07
             - a13 * a03 * a06

        m06' = -a00 * a06 * a15
             +  a00 * a07 * a14
             +  a04 * a02 * a15
             -  a04 * a03 * a14
             -  a12 * a02 * a07
             +  a12 * a03 * a06

        m10' = a00 * a05 * a15
             - a00 * a07 * a13
             - a04 * a01 * a15
             + a04 * a03 * a13
             + a12 * a01 * a07
             - a12 * a03 * a05

        m14' = -a00 * a05 * a14
             +  a00 * a06 * a13
             +  a04 * a01 * a14
             -  a04 * a02 * a13
             -  a12 * a01 * a06
             +  a12 * a02 * a05

        m03' = -a01 * a06 * a11
             +  a01 * a07 * a10
             +  a05 * a02 * a11
             -  a05 * a03 * a10
             -  a09 * a02 * a07
             +  a09 * a03 * a06

        m07' = a00 * a06 * a11
             - a00 * a07 * a10
             - a04 * a02 * a11
             + a04 * a03 * a10
             + a08 * a02 * a07
             - a08 * a03 * a06

        m11' = -a00 * a05 * a11
             +  a00 * a07 * a09
             +  a04 * a01 * a11
             -  a04 * a03 * a09
             -  a08 * a01 * a07
             +  a08 * a03 * a05

        m15' = a00 * a05 * a10
             - a00 * a06 * a09
             - a04 * a01 * a10
             + a04 * a02 * a09
             + a08 * a01 * a06
             - a08 * a02 * a05

        det' = a00 * m00' + a01 * m04' + a02 * m08' + a03 * m12'
    in
        if det' == 0 then Spear.Math.Matrix4.id
        else
            let det = (1::Float) / det'
            in mat4
                (m00' * det) (m04' * det) (m08' * det) (m12' * det)
                (m01' * det) (m05' * det) (m09' * det) (m13' * det)
                (m02' * det) (m06' * det) (m10' * det) (m14' * det)
                (m03' * det) (m07' * det) (m11' * det) (m15' * det)


-- | Transform the given vector in 3D space with the given matrix.
mul :: Float -> Matrix4 -> Vector3 -> Vector3
mul w m v = vec3 x' y' z'
    where
        v' = vec4 (x v) (y v) (z v) w
        x' = row0 m `dot` v'
        y' = row1 m `dot` v'
        z' = row2 m `dot` v'

-- | Transform the given point vector in 3D space with the given matrix.
mulp :: Matrix4 -> Vector3 -> Vector3
mulp = mul 1

-- | Transform the given directional vector in 3D space with the given matrix.
muld :: Matrix4 -> Vector3 -> Vector3
muld = mul 0

-- | Transform the given vector with the given matrix.
--
-- The vector is brought from homogeneous space to 3D space by performing a
-- perspective divide.
mul' :: Float -> Matrix4 -> Vector3 -> Vector3
mul' w m v = vec3 (x'/w') (y'/w') (z'/w')
    where
        v' = vec4 (x v) (y v) (z v) w
        x' = row0 m `dot` v'
        y' = row1 m `dot` v'
        z' = row2 m `dot` v'
        w' = row3 m `dot` v'
