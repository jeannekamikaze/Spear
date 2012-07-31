module Spear.GLSL.Buffer
(
    GLBuffer
,   TargetBuffer(..)
,   BufferUsage(..)
,   newBuffer
,   releaseBuffer
,   bindBuffer
,   bufferData
,   withGLBuffer
)
where


import Spear.Setup
import Spear.GLSL.Management

import Graphics.Rendering.OpenGL.Raw.Core31
import Control.Monad.Trans.Class (lift)
import Data.StateVar
import Foreign.Ptr
import Foreign.Marshal.Utils as Foreign (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Unsafe.Coerce


-- | Represents an OpenGL buffer.
data GLBuffer = GLBuffer
    { getBuffer :: GLuint
    , rkey      :: Resource
    }


-- | Represents a target buffer.
data TargetBuffer
    = ArrayBuffer
    | ElementArrayBuffer
    | PixelPackBuffer
    | PixelUnpackBuffer
    deriving (Eq, Show)
    
    
fromTarget :: TargetBuffer -> GLenum
fromTarget ArrayBuffer        = gl_ARRAY_BUFFER
fromTarget ElementArrayBuffer = gl_ELEMENT_ARRAY_BUFFER
fromTarget PixelPackBuffer    = gl_PIXEL_PACK_BUFFER
fromTarget PixelUnpackBuffer  = gl_PIXEL_UNPACK_BUFFER


-- | Represents a type of buffer usage.
data BufferUsage
    = StreamDraw
    | StreamRead
    | StreamCopy
    | StaticDraw
    | StaticRead
    | StaticCopy
    | DynamicDraw
    | DynamicRead
    | DynamicCopy
    deriving (Eq, Show)


fromUsage :: BufferUsage -> GLenum
fromUsage StreamDraw  = gl_STREAM_DRAW
fromUsage StreamRead  = gl_STREAM_READ
fromUsage StreamCopy  = gl_STREAM_COPY
fromUsage StaticDraw  = gl_STATIC_DRAW
fromUsage StaticRead  = gl_STATIC_READ
fromUsage StaticCopy  = gl_STATIC_COPY
fromUsage DynamicDraw = gl_DYNAMIC_DRAW
fromUsage DynamicRead = gl_DYNAMIC_READ
fromUsage DynamicCopy = gl_DYNAMIC_COPY


-- | Create a 'GLBuffer'.
newBuffer :: Setup GLBuffer
newBuffer = do
    h <- setupIO . alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    
    rkey <- register $ deleteBuffer h
    return $ GLBuffer h rkey


-- | Release the given 'GLBuffer'.
releaseBuffer :: GLBuffer -> Setup ()
releaseBuffer = release . rkey


-- | Delete the given 'GLBuffer'.
deleteBuffer :: GLuint -> IO ()
deleteBuffer buf = Foreign.with buf $ glDeleteBuffers 1


-- | Bind the given 'GLBuffer'.
bindBuffer :: GLBuffer -> TargetBuffer -> IO ()
bindBuffer buf target = glBindBuffer (fromTarget target) $ getBuffer buf


-- | Set buffer data.
bufferData :: TargetBuffer -> Int -> Ptr a -> BufferUsage -> IO ()
bufferData target n bufData usage = glBufferData (fromTarget target) (unsafeCoerce n) bufData (fromUsage usage)


-- | Apply the given function the 'GLBuffer''s id.
withGLBuffer :: GLBuffer -> (GLuint -> a) -> a
withGLBuffer buf f = f $ getBuffer buf

