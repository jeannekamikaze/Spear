module Spear.Math.Matrix3
(
    Matrix3
    -- * Accessors
,   m00, m01, m02
,   m10, m11, m12
,   m20, m21, m22
,   col0, col1, col2
,   row0, row1, row2
    -- * Construction
,   mat3
,   mat3fromVec
,   Spear.Math.Matrix3.id
    -- * Transformations
    -- ** Rotation
,   rotX
,   rotY
,   rotZ
,   axisAngle
    -- ** Scale
,   Spear.Math.Matrix3.scale
,   scalev
    -- ** Reflection
,   reflectX
,   reflectY
,   reflectZ
    -- * Operations
,   transpose
,   Spear.Math.Matrix3.zipWith
,   Spear.Math.Matrix3.map
,   mul
--,   inverse
)
where


import Spear.Math.Vector3 as Vector3
import Spear.Math.Vector4 as Vector4

import Foreign.Storable


-- | Represents a 3x3 column major matrix.
data Matrix3 = Matrix3
    { m00 :: {-# UNPACK #-} !Float, m10 :: {-# UNPACK #-} !Float, m20 :: {-# UNPACK #-} !Float
    , m01 :: {-# UNPACK #-} !Float, m11 :: {-# UNPACK #-} !Float, m21 :: {-# UNPACK #-} !Float
    , m02 :: {-# UNPACK #-} !Float, m12 :: {-# UNPACK #-} !Float, m22 :: {-# UNPACK #-} !Float
    }


instance Show Matrix3 where
    
    show (Matrix3 m00 m10 m20 m01 m11 m21 m02 m12 m22) =
        show' m00 ++ ", " ++ show' m10 ++ ", " ++ show' m20 ++ "\n" ++
        show' m01 ++ ", " ++ show' m11 ++ ", " ++ show' m21 ++ "\n" ++
        show' m02 ++ ", " ++ show' m12 ++ ", " ++ show' m22 ++ "\n"
        where
            show' f = if abs f < 0.0000001 then "0" else show f


instance Num Matrix3 where
    (Matrix3 a00 a01 a02 a03 a04 a05 a06 a07 a08)
        + (Matrix3 b00 b01 b02 b03 b04 b05 b06 b07 b08)
            = Matrix3 (a00 + b00) (a01 + b01) (a02 + b02)
                      (a03 + b03) (a04 + b04) (a05 + b05)
                      (a06 + b06) (a07 + b07) (a08 + b08)
    
    (Matrix3 a00 a01 a02 a03 a04 a05 a06 a07 a08)
        - (Matrix3 b00 b01 b02 b03 b04 b05 b06 b07 b08)
            = Matrix3 (a00 - b00) (a01 - b01) (a02 - b02)
                      (a03 - b03) (a04 - b04) (a05 - b05)
                      (a06 - b06) (a07 - b07) (a08 - b08)
    
    (Matrix3 a00 a10 a20 a01 a11 a21 a02 a12 a22)
        * (Matrix3 b00 b10 b20 b01 b11 b21 b02 b12 b22)
            = Matrix3 (a00 * b00 + a10 * b01 + a20 * b02)
                      (a00 * b10 + a10 * b11 + a20 * b12)
                      (a00 * b20 + a10 * b21 + a20 * b22)
                      
                      (a01 * b00 + a11 * b01 + a21 * b02)
                      (a01 * b10 + a11 * b11 + a21 * b12)
                      (a01 * b20 + a11 * b21 + a21 * b22)
                      
                      (a02 * b00 + a12 * b01 + a22 * b02)
                      (a02 * b10 + a12 * b11 + a22 * b12)
                      (a02 * b20 + a12 * b21 + a22 * b22)
    
    abs = Spear.Math.Matrix3.map abs
    
    signum = Spear.Math.Matrix3.map signum
    
    fromInteger i = mat3 i' i' i' i' i' i' i' i' i' where i' = fromInteger i
    
    
instance Storable Matrix3 where
    sizeOf    _ = 36
    alignment _ = 4
    
    peek ptr = do
        a00 <- peekByteOff ptr 0;  a01 <- peekByteOff ptr 4;  a02 <- peekByteOff ptr 8;
        a10 <- peekByteOff ptr 12; a11 <- peekByteOff ptr 16; a12 <- peekByteOff ptr 20;
        a20 <- peekByteOff ptr 24; a21 <- peekByteOff ptr 28; a22 <- peekByteOff ptr 32;
        
        return $ Matrix3 a00 a10 a20
                         a01 a11 a21
                         a02 a12 a22
    
    poke ptr (Matrix3 a00 a01 a02
                      a10 a11 a12
                      a20 a21 a22) = do
        pokeByteOff ptr 0  a00; pokeByteOff ptr 4  a01; pokeByteOff ptr 8  a02;
        pokeByteOff ptr 12 a10; pokeByteOff ptr 16 a11; pokeByteOff ptr 20 a12;
        pokeByteOff ptr 24 a20; pokeByteOff ptr 28 a21; pokeByteOff ptr 32 a22;


col0 (Matrix3 a00 _   _   a10 _   _   a20 _   _  ) = vec3 a00 a10 a20
col1 (Matrix3 _   a01 _   _   a11 _   _   a21 _  ) = vec3 a01 a11 a21
col2 (Matrix3 _   _   a02 _   _   a12 _   _   a22) = vec3 a02 a12 a22


row0 (Matrix3 a00 a01 a02 _   _   _   _   _   _  ) = vec3 a00 a01 a02
row1 (Matrix3 _   _   _   a10 a11 a12 _   _   _  ) = vec3 a10 a11 a12
row2 (Matrix3 _   _   _   _   _   _   a20 a21 a22) = vec3 a20 a21 a22


-- | Build a 'Matrix3' from the specified values.
mat3 :: Float -> Float -> Float ->
        Float -> Float -> Float ->
        Float -> Float -> Float -> Matrix3
mat3 m00 m01 m02 m10 m11 m12 m20 m21 m22 = Matrix3
    m00 m10 m20
    m01 m11 m21
    m02 m12 m22


-- | Build a 'Matrix3' from three vectors in 3D.
mat3fromVec :: Vector3 -> Vector3 -> Vector3 -> Matrix3
mat3fromVec v0 v1 v2 = Matrix3
    (Vector3.x v0) (Vector3.x v1) (Vector3.x v2)
    (Vector3.y v0) (Vector3.y v1) (Vector3.y v2)
    (Vector3.z v0) (Vector3.z v1) (Vector3.z v2)
 

-- | Zip two 'Matrix3' together with the specified function.
zipWith :: (Float -> Float -> Float) -> Matrix3 -> Matrix3 -> Matrix3
zipWith f a b = Matrix3
    (f (m00 a) (m00 b)) (f (m10 a) (m10 b)) (f (m20 a) (m20 b))
    (f (m01 a) (m01 b)) (f (m11 a) (m11 b)) (f (m21 a) (m21 b))
    (f (m02 a) (m02 b)) (f (m12 a) (m12 b)) (f (m22 a) (m22 b))


-- | Map the specified function to the specified 'Matrix3'.
map :: (Float -> Float) -> Matrix3 -> Matrix3
map f m = Matrix3
    (f . m00 $ m) (f . m10 $ m) (f . m20 $ m)
    (f . m01 $ m) (f . m11 $ m) (f . m21 $ m)
    (f . m02 $ m) (f . m12 $ m) (f . m22 $ m)


-- | Return the identity matrix.
id :: Matrix3
id = mat3
    1   0   0
    0   1   0
    0   0   1


-- | Create a rotation matrix rotating about the X axis.
-- The given angle must be in degrees.
rotX :: Float -> Matrix3
rotX angle = mat3
    1   0   0
    0   c   (-s)
    0   s   c
    where
        s = sin . fromDeg $ angle
        c = cos . fromDeg $ angle


-- | Create a rotation matrix rotating about the Y axis.
-- The given angle must be in degrees.
rotY :: Float -> Matrix3
rotY angle = mat3
    c    0    s
    0    1    0
    (-s) 0    c
    where
        s = sin . fromDeg $ angle
        c = cos . fromDeg $ angle


-- | Create a rotation matrix rotating about the Z axis.
-- The given angle must be in degrees.
rotZ :: Float -> Matrix3
rotZ angle = mat3
    c    (-s)  0
    s    c     0
    0    0     1
    where
        s = sin . fromDeg $ angle
        c = cos . fromDeg $ angle


-- | Create a rotation matrix rotating about the specified axis.
-- The given angle must be in degrees.
axisAngle :: Vector3 -> Float -> Matrix3
axisAngle v angle = mat3
    (c + omc*x^2) (omc*xy-sz) (omc*xz+sy)
    (omc*xy+sz)   (c+omc*y^2) (omc*yz-sx)
    (omc*xz-sy)   (omc*yz+sx) (c+omc*z^2)
    where
        x = Vector3.x v
        y = Vector3.y v
        z = Vector3.z v
        s   = sin . fromDeg $ angle
        c   = cos . fromDeg $ angle
        xy  = x*y
        xz  = x*z
        yz  = y*z
        sx  = s*x
        sy  = s*y
        sz  = s*z
        omc = 1 - c


-- | Create a scale matrix.
scale :: Float -> Float -> Float -> Matrix3
scale sx sy sz = mat3
    sx  0   0
    0   sy  0
    0   0   sz
    
    
-- | Create a scale matrix.
scalev :: Vector3 -> Matrix3
scalev v = mat3
    sx  0   0
    0   sy  0
    0   0   sz
        where
            sx = Vector3.x v
            sy = Vector3.y v
            sz = Vector3.z v


-- | Create an X reflection matrix.
reflectX :: Matrix3
reflectX = mat3
    (-1)  0   0
    0     1   0
    0     0   1


-- | Create a Y reflection matrix.
reflectY :: Matrix3
reflectY = mat3
    1   0     0
    0   (-1)  0
    0   0     1


-- | Create a Z reflection matrix.
reflectZ :: Matrix3
reflectZ = mat3
    1   0   0
    0   1   0
    0   0   (-1)

    
-- | Transpose the specified matrix.
transpose :: Matrix3 -> Matrix3
transpose m = mat3
    (m00 m) (m01 m) (m02 m)
    (m10 m) (m11 m) (m12 m)
    (m20 m) (m21 m) (m22 m)


-- | Transform the given vector in 3D space with the given matrix.
mul :: Matrix3 -> Vector3 -> Vector3
mul m v = vec3 x' y' z'
    where
        v' = vec3 (Vector3.x v) (Vector3.y v) (Vector3.z v)
        x' = row0 m `Vector3.dot` v'
        y' = row1 m `Vector3.dot` v'
        z' = row2 m `Vector3.dot` v'


-- | Invert the given 'Matrix3'.
{-inverse :: Matrix3 -> Matrix3
inverse mat = -}


fromDeg :: (Floating a) => a -> a
fromDeg = (*pi) . (/180)

