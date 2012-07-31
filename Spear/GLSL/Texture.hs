module Spear.GLSL.Texture
(
    Texture
,   SettableStateVar
,   GLenum
,   ($)
    -- * Creation and destruction
,   newTexture
,   releaseTexture
    -- * Manipulation
,   bindTexture
,   loadTextureData
,   texParami
,   texParamf
,   activeTexture
)
where


import Spear.Setup

import Data.StateVar
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable (peek)
import Graphics.Rendering.OpenGL.Raw.Core31
import Unsafe.Coerce (unsafeCoerce)


-- | Represents a texture resource.
data Texture = Texture
    { getTex :: GLuint
    , rkey   :: Resource
    }


instance Eq Texture where
    t1 == t2 = getTex t1 == getTex t2


instance Ord Texture where
    t1 < t2 = getTex t1 < getTex t2


-- | Create a new 'Texture'.
newTexture :: Setup Texture
newTexture = do
    tex <- setupIO . alloca $ \ptr -> do
        glGenTextures 1 ptr
        peek ptr
    
    rkey <- register $ deleteTexture tex
    return $ Texture tex rkey


-- | Release the given 'Texture'.
releaseTexture :: Texture -> Setup ()
releaseTexture = release . rkey


-- | Delete the given 'Texture'.
deleteTexture :: GLuint -> IO ()
--deleteTexture tex = with tex $ glDeleteTextures 1
deleteTexture tex = do
    putStrLn $ "Releasing texture " ++ show tex
    with tex $ glDeleteTextures 1


-- | Bind the given 'Texture'.
bindTexture :: Texture -> IO ()
bindTexture = glBindTexture gl_TEXTURE_2D . getTex


-- | Load data onto the bound 'Texture'.
loadTextureData :: GLenum
                -> Int -- ^ Target
                -> Int -- ^ Level
                -> Int -- ^ Internal format
                -> Int -- ^ Width
                -> Int -- ^ Height
                -> GLenum -- ^ Border
                -> GLenum -- ^ Texture type
                -> Ptr a -- ^ Texture data
                -> IO ()
loadTextureData target level internalFormat width height border format texType texData = do
    glTexImage2D target
                 (fromIntegral level)
                 (fromIntegral internalFormat)
                 (fromIntegral width)
                 (fromIntegral height)
                 (fromIntegral border)
                 (fromIntegral format)
                 texType
                 texData


-- | Set the bound texture's given parameter to the given value.
texParami :: GLenum -> GLenum -> SettableStateVar GLenum
texParami target param = makeSettableStateVar $ \val -> glTexParameteri target param $ fromIntegral . fromEnum $ val


-- | Set the bound texture's given parameter to the given value.
texParamf :: GLenum -> GLenum -> SettableStateVar Float
texParamf target param = makeSettableStateVar $ \val -> glTexParameterf target param (unsafeCoerce val)


-- | Set the active texture unit.
activeTexture :: SettableStateVar GLenum
activeTexture = makeSettableStateVar glActiveTexture
