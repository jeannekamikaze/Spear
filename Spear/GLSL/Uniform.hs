module Spear.GLSL.Uniform
(
    uniformVec3
,   uniformVec4
,   uniformMat3
,   uniformMat4
,   uniformfl
,   uniformil
)
where


import Spear.GLSL.Management
import Spear.Math.Matrix3 (Matrix3)
import Spear.Math.Matrix4 (Matrix4)
import Spear.Math.Vector3 as V3
import Spear.Math.Vector4 as V4

import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils
import Graphics.Rendering.OpenGL.Raw.Core31
import Unsafe.Coerce


uniformVec3 :: GLint -> Vector3 -> IO ()
uniformVec3 loc v = glUniform3f loc x' y' z'
    where x' = unsafeCoerce $ V3.x v
          y' = unsafeCoerce $ V3.y v
          z' = unsafeCoerce $ V3.z v
    

uniformVec4 :: GLint -> Vector4 -> IO ()
uniformVec4 loc v = glUniform4f loc x' y' z' w'
    where x' = unsafeCoerce $ V4.x v
          y' = unsafeCoerce $ V4.y v
          z' = unsafeCoerce $ V4.z v
          w' = unsafeCoerce $ V4.w v


uniformMat3 :: GLint -> Matrix3 -> IO ()
uniformMat3 loc mat =
    with mat $ \ptrMat ->
        glUniformMatrix3fv loc 1 (toEnum 0) (unsafeCoerce ptrMat)


uniformMat4 :: GLint -> Matrix4 -> IO ()
uniformMat4 loc mat =
    with mat $ \ptrMat ->
        glUniformMatrix4fv loc 1 (toEnum 0) (unsafeCoerce ptrMat)


uniformfl :: GLint -> [GLfloat] -> IO ()
uniformfl loc vals = withArray vals $ \ptr ->
    case length vals of
        1 -> glUniform1fv loc 1 ptr
        2 -> glUniform2fv loc 1 ptr
        3 -> glUniform3fv loc 1 ptr
        4 -> glUniform4fv loc 1 ptr


uniformil :: GLint -> [GLint] -> IO ()
uniformil loc vals = withArray vals $ \ptr ->
    case length vals of
        1 -> glUniform1iv loc 1 ptr
        2 -> glUniform2iv loc 1 ptr
        3 -> glUniform3iv loc 1 ptr
        4 -> glUniform4iv loc 1 ptr
