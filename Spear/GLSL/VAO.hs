module Spear.GLSL.VAO
(
    VAO
    -- * Creation and destruction
,   newVAO
,   releaseVAO
    -- * Manipulation
,   bindVAO
,   enableVAOAttrib
,   attribVAOPointer
    -- * Rendering
,   drawArrays
,   drawElements
)
where


import Spear.Setup
import Control.Monad.Trans.Class (lift)
import Foreign.Marshal.Utils as Foreign (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.Ptr
import Unsafe.Coerce
import Graphics.Rendering.OpenGL.Raw.Core31


-- | Represents a vertex array object.
data VAO = VAO
    { getVAO :: GLuint
    , rkey   :: Resource
    }


instance Eq VAO where
    vao1 == vao2 = getVAO vao1 == getVAO vao2


instance Ord VAO where
    vao1 < vao2 = getVAO vao1 < getVAO vao2


-- | Create a new 'VAO'.
newVAO :: Setup VAO
newVAO = do
    h <- setupIO . alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    
    rkey <- register $ deleteVAO h
    return $ VAO h rkey


-- | Release the given 'VAO'.
releaseVAO :: VAO -> Setup ()
releaseVAO = release . rkey


-- | Delete the given 'VAO'.
deleteVAO :: GLuint -> IO ()
deleteVAO vao = Foreign.with vao $ glDeleteVertexArrays 1


-- | Bind the given 'VAO'.
bindVAO :: VAO -> IO ()
bindVAO = glBindVertexArray . getVAO


-- | Enable the given vertex attribute of the bound 'VAO'.
enableVAOAttrib :: GLuint -> IO ()
enableVAOAttrib = glEnableVertexAttribArray


-- | Bind the bound buffer to the given point.
attribVAOPointer :: GLuint -> GLint -> GLenum -> Bool -> GLsizei -> Int -> IO ()
attribVAOPointer idx ncomp dattype normalise stride off =
    glVertexAttribPointer idx ncomp dattype (unsafeCoerce normalise) stride (unsafeCoerce off)


-- | Draw the bound 'VAO'.
drawArrays :: GLenum -> Int -> Int -> IO ()
drawArrays mode first count = glDrawArrays mode (unsafeCoerce first) (unsafeCoerce count)


-- | Draw the bound 'VAO', indexed mode.
drawElements :: GLenum -> Int -> GLenum -> Ptr a -> IO ()
drawElements mode count t idxs = glDrawElements mode (unsafeCoerce count) t idxs

