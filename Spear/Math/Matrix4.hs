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
    -- * Construction
,   mat4
,   mat4fromVec
,   transform
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
    -- * Operations
,   Spear.Math.Matrix4.zipWith
,   Spear.Math.Matrix4.map
,   transpose
,   inverseTransform
,   mul
,   mulp
,   muld
)
where


import Spear.Math.Vector3 as Vector3
import Spear.Math.Vector4 as Vector4

import Foreign.Storable


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


instance Num Matrix4 where
    (Matrix4 a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15)
        + (Matrix4 b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15)
            = Matrix4 (a00 + b00) (a01 + b01) (a02 + b02) (a03 + b03)
                      (a04 + b04) (a05 + b05) (a06 + b06) (a07 + b07)
                      (a08 + b08) (a09 + b09) (a10 + b10) (a11 + b11)
                      (a12 + b12) (a13 + b13) (a14 + b14) (a15 + b15)
    
    (Matrix4 a00 a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15)
        - (Matrix4 b00 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12 b13 b14 b15)
            = Matrix4 (a00 - b00) (a01 - b01) (a02 - b02) (a03 - b03)
                      (a04 - b04) (a05 - b05) (a06 - b06) (a07 - b07)
                      (a08 - b08) (a09 - b09) (a10 - b10) (a11 - b11)
                      (a12 - b12) (a13 - b13) (a14 - b14) (a15 - b15)
    
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
    
    abs = Spear.Math.Matrix4.map abs
    
    signum = Spear.Math.Matrix4.map signum
    
    fromInteger i = mat4 i' i' i' i' i' i' i' i' i' i' i' i' i' i' i' i' where i' = fromInteger i
    
    
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


-- | Build a matrix from the specified values.
mat4 = Matrix4


-- | Build a matrix from four vectors in 4D.
mat4fromVec :: Vector4 -> Vector4 -> Vector4 -> Vector4 -> Matrix4
mat4fromVec v0 v1 v2 v3 = Matrix4
    (Vector4.x v0) (Vector4.x v1) (Vector4.x v2) (Vector4.x v3)
    (Vector4.y v0) (Vector4.y v1) (Vector4.y v2) (Vector4.y v3)
    (Vector4.z v0) (Vector4.z v1) (Vector4.z v2) (Vector4.z v3)
    (Vector4.w v0) (Vector4.w v1) (Vector4.w v2) (Vector4.w v3)


-- | Build a transformation 'Matrix4' from the given vectors.
transform :: Vector3 -- ^ Right vector.
          -> Vector3 -- ^ Up vector.
          -> Vector3 -- ^ Forward vector.
          -> Vector3 -- ^ Position.
          -> Matrix4

transform right up fwd pos = mat4
    (Vector3.x right) (Vector3.x up) (Vector3.x fwd) (Vector3.x pos)
    (Vector3.y right) (Vector3.y up) (Vector3.y fwd) (Vector3.y pos)
    (Vector3.z right) (Vector3.z up) (Vector3.z fwd) (Vector3.z pos)
    0                 0              0               1


-- | Build a transformation 'Matrix4' defined by the given position and target.
lookAt :: Vector3 -- ^ Eye position.
       -> Vector3 -- ^ Target point.
       -> Matrix4

lookAt pos target =
        let fwd = Vector3.normalise $ target - pos
            r    = fwd `cross` Vector3.unity
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
    1    0    0    (Vector3.x v)
    0    1    0    (Vector3.y v)
    0    0    1    (Vector3.z v)
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
        s = sin . toRAD $ angle
        c = cos . toRAD $ angle


-- | Create a rotation matrix rotating about the Y axis.
-- The given angle must be in degrees.
rotY :: Float -> Matrix4
rotY angle = mat4
    c    0    s    0
    0    1    0    0
    (-s) 0    c    0
    0    0    0    1
    where
        s = sin . toRAD $ angle
        c = cos . toRAD $ angle


-- | Create a rotation matrix rotating about the Z axis.
-- The given angle must be in degrees.
rotZ :: Float -> Matrix4
rotZ angle = mat4
    c    (-s) 0    0
    s    c    0    0
    0    0    1    0
    0    0    0    1
    where
        s = sin . toRAD $ angle
        c = cos . toRAD $ angle


-- | Create a rotation matrix rotating about the specified axis.
-- The given angle must be in degrees.
axisAngle :: Vector3 -> Float -> Matrix4
axisAngle v angle = mat4
    (c + omc*x^2) (omc*xy-sz) (omc*xz+sy) 0
    (omc*xy+sz)   (c+omc*y^2) (omc*yz-sx) 0
    (omc*xz-sy)   (omc*yz+sx) (c+omc*z^2) 0
     0             0           0          1
    where
        x = Vector3.x v
        y = Vector3.y v
        z = Vector3.z v
        s   = sin . toRAD $ angle
        c   = cos . toRAD $ angle
        xy  = x*y
        xz  = x*z
        yz  = y*z
        sx  = s*x
        sy  = s*y
        sz  = s*z
        omc = 1 - c


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
            sx = Vector3.x v
            sy = Vector3.y v
            sz = Vector3.z v


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
    let f = 1 / tan (toRAD fovy / 2)
        a = near - far
    in mat4
        (f/r) 0    0              0
        0     f    0              0
        0     0    ((near+far)/a) (2*near*far/a)
        0     0    (-1)           0

    
-- | Transpose the specified matrix.
transpose :: Matrix4 -> Matrix4
transpose m = mat4
    (m00 m) (m01 m) (m02 m) (m03 m)
    (m10 m) (m11 m) (m12 m) (m13 m)
    (m20 m) (m21 m) (m22 m) (m23 m)
    (m30 m) (m31 m) (m32 m) (m33 m)


-- | Invert the given transformation matrix.
inverseTransform :: Matrix4 -> Matrix4
inverseTransform mat = mat4fromVec u v w p where
    v0 = row0 mat
    v1 = row1 mat
    v2 = row2 mat
    u  = vec4 (Vector4.x v0) (Vector4.y v0) (Vector4.z v0) 0
    v  = vec4 (Vector4.x v1) (Vector4.y v1) (Vector4.z v1) 0
    w  = vec4 (Vector4.x v2) (Vector4.y v2) (Vector4.z v2) 0
    p  = vec4 tdotu tdotv tdotw 1
    t  = -(col3 mat)
    tdotu = t `Vector4.dot` col0 mat
    tdotv = t `Vector4.dot` col1 mat
    tdotw = t `Vector4.dot` col2 mat


-- | Transform the given vector in 3D space with the given matrix.
mul :: Float -> Matrix4 -> Vector3 -> Vector3
mul w m v = vec3 x' y' z'
    where
        v' = vec4 (Vector3.x v) (Vector3.y v) (Vector3.z v) w
        x' = row0 m `Vector4.dot` v'
        y' = row1 m `Vector4.dot` v'
        z' = row2 m `Vector4.dot` v'


-- | Transform the given point vector in 3D space with the given matrix.
mulp :: Matrix4 -> Vector3 -> Vector3
mulp = mul 1


-- | Transform the given directional vector in 3D space with the given matrix.
muld :: Matrix4 -> Vector3 -> Vector3
muld = mul 0


toRAD = (*pi) . (/180)
