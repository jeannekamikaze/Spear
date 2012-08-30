module Spear.GLSL
(
    -- * General Management
    GLSLShader
,   GLSLProgram
,   ShaderType(..)
    -- ** Programs
,   newProgram
,   releaseProgram
,   linkProgram
,   useProgram
,   withGLSLProgram
    -- ** Shaders
,   attachShader
,   detachShader
,   loadShader
,   newShader
,   releaseShader
    -- *** Source loading
,   loadSource
,   shaderSource
,   readSource
,   compile
    -- ** Locations
,   attribLocation
,   fragLocation
,   uniformLocation
    -- ** Uniforms
,   uniformVec2
,   uniformVec3
,   uniformVec4
,   uniformMat3
,   uniformMat4
,   uniformfl
,   uniformil
    -- ** Helper functions
,   ($=)
,   Data.StateVar.get

    -- * VAOs
,   VAO
    -- ** Creation and destruction
,   newVAO
,   releaseVAO
    -- ** Manipulation
,   bindVAO
,   enableVAOAttrib
,   attribVAOPointer
    -- ** Rendering
,   drawArrays
,   drawElements

    -- * Buffers
,   GLBuffer
,   TargetBuffer(..)
,   BufferUsage(..)
    -- ** Creation and destruction
,   newBuffer
,   releaseBuffer
    -- ** Manipulation
,   bindBuffer
,   bufferData
,   withGLBuffer

    -- * Textures
,   Texture
,   SettableStateVar
,   GLenum
,   ($)
    -- ** Creation and destruction
,   newTexture
,   loadTextureImage
,   releaseTexture
    -- ** Manipulation
,   bindTexture
,   loadTextureData
,   texParami
,   texParamf
,   activeTexture

    -- * Error Handling
,   getGLError
,   printGLError
,   assertGL
)
where


import Spear.Assets.Image
import Spear.Math.Matrix3 (Matrix3)
import Spear.Math.Matrix4 (Matrix4)
import Spear.Math.Vector2 as V2
import Spear.Math.Vector3 as V3
import Spear.Math.Vector4 as V4
import Spear.Setup

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as B
import Data.StateVar
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils as Foreign (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (peek)
import Graphics.Rendering.OpenGL.Raw.Core31
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.IO (hPutStrLn, stderr)
import Unsafe.Coerce


--
-- MANAGEMENT
--


-- | A GLSL shader handle.
data GLSLShader  = GLSLShader
    { getShader    :: GLuint
    , getShaderKey :: Resource
    }


-- | A GLSL program handle.
data GLSLProgram = GLSLProgram
    { getProgram    :: GLuint
    , getProgramKey :: Resource
    }
    
    
-- | Supported shader types.
data ShaderType = VertexShader | FragmentShader deriving (Eq, Show)


toGLShader :: ShaderType -> GLenum
toGLShader VertexShader   = gl_VERTEX_SHADER
toGLShader FragmentShader = gl_FRAGMENT_SHADER


-- | Apply the given function to the program's id.
withGLSLProgram :: GLSLProgram -> (GLuint -> a) -> a
withGLSLProgram prog f = f $ getProgram prog


-- | Get the location of the given uniform variable within the given program.
uniformLocation :: GLSLProgram -> String -> GettableStateVar GLint
uniformLocation prog var = makeGettableStateVar get
    where
        get = withCString var $ \str -> glGetUniformLocation (getProgram prog) (unsafeCoerce str)


-- | Get or set the location of the given variable to a fragment shader colour number.
fragLocation :: GLSLProgram -> String -> StateVar GLint
fragLocation prog var = makeStateVar get set
    where
        get     = withCString var $ \str -> glGetFragDataLocation (getProgram prog) (unsafeCoerce str)
        set idx = withCString var $ \str ->
            glBindFragDataLocation (getProgram prog) (unsafeCoerce idx) (unsafeCoerce str)


-- | Get or set the location of the given attribute within the given program.
attribLocation :: GLSLProgram -> String -> StateVar GLint
attribLocation prog var = makeStateVar get set
    where
        get     = withCString var $ \str -> glGetAttribLocation  (getProgram prog) (unsafeCoerce str)
        set idx = withCString var $ \str ->
            glBindAttribLocation (getProgram prog) (unsafeCoerce idx) (unsafeCoerce str)


-- | Create a new program.
newProgram :: [GLSLShader] -> Setup GLSLProgram
newProgram shaders = do
    h <- setupIO glCreateProgram
    when (h == 0) $ setupError "glCreateProgram failed"
    rkey <- register $ deleteProgram h
    let program = GLSLProgram h rkey
    
    mapM_ (setupIO . attachShader program) shaders
    linkProgram program
    
    return program


-- | Release the program.
releaseProgram :: GLSLProgram -> Setup ()
releaseProgram = release . getProgramKey


-- | Delete the program.
deleteProgram :: GLuint -> IO ()
--deleteProgram = glDeleteProgram
deleteProgram prog = do
    putStrLn $ "Deleting shader program " ++ show prog
    glDeleteProgram prog


-- | Link the program.
linkProgram :: GLSLProgram -> Setup ()
linkProgram prog = do
    let h = getProgram prog
    err <- setupIO $ do
        glLinkProgram h
        alloca $ \statptr -> do
            glGetProgramiv h gl_LINK_STATUS statptr
            status <- peek statptr
            case status of
                0 -> getStatus glGetProgramiv glGetProgramInfoLog h
                _ -> return ""
    
    case length err of
        0 -> return ()
        _ -> setupError err


-- | Use the program.
useProgram :: GLSLProgram -> IO ()
useProgram prog = glUseProgram $ getProgram prog


-- | Attach the given shader to the given program.
attachShader :: GLSLProgram -> GLSLShader -> IO ()
attachShader prog shader = glAttachShader (getProgram prog) (getShader shader)


-- | Detach the given GLSL from the given program.
detachShader :: GLSLProgram -> GLSLShader -> IO ()
detachShader prog shader = glDetachShader (getProgram prog) (getShader shader)


-- | Load a shader from the file specified by the given string.
--
-- This function creates a new shader. To load source code into an existing shader,
-- see 'loadSource', 'shaderSource' and 'readSource'.
loadShader :: FilePath -> ShaderType -> Setup GLSLShader
loadShader file shaderType = do
    shader <- newShader shaderType
    loadSource file shader
    compile file shader
    return shader


-- | Create a new shader.
newShader :: ShaderType -> Setup GLSLShader
newShader shaderType = do
    h <- setupIO $ glCreateShader (toGLShader shaderType)
    case h of
        0 -> setupError "glCreateShader failed"
        _ -> do
            rkey <- register $ deleteShader h
            return $ GLSLShader h rkey


-- | Release the shader.
releaseShader :: GLSLShader -> Setup ()
releaseShader = release . getShaderKey


-- | Free the shader.
deleteShader :: GLuint -> IO ()
--deleteShader = glDeleteShader
deleteShader shader = do
    putStrLn $ "Deleting shader " ++ show shader
    glDeleteShader shader


-- | Load a shader source from the file specified by the given string
-- into the shader.
loadSource :: FilePath -> GLSLShader -> Setup ()
loadSource file h = do
    exists <- setupIO $ doesFileExist file
    case exists of
        False -> setupError "the specified shader file does not exist"
        True  -> setupIO $ do
            code <- readSource file
            withCString code $ shaderSource h


-- | Load the given shader source into the shader.
shaderSource :: GLSLShader -> CString -> IO ()
shaderSource shader str =
    let ptr = unsafeCoerce str
    in  withArray [ptr] $ flip (glShaderSource (getShader shader) 1) nullPtr


-- | Compile the shader.
compile :: FilePath -> GLSLShader -> Setup ()
compile file shader = do
    let h = getShader shader
    
    -- Compile
    setupIO $ glCompileShader h
    
    -- Verify status
    err <- setupIO $ alloca $ \statusPtr -> do
        glGetShaderiv h gl_COMPILE_STATUS statusPtr
        result <- peek statusPtr
        case result of
            0 -> getStatus glGetShaderiv glGetShaderInfoLog h
            _ -> return ""
    
    case length err of
        0 -> return ()
        _ -> setupError $ "Unable to compile shader " ++ file ++ ":\n" ++ err
        
        
type StatusCall = GLuint -> GLenum -> Ptr GLint -> IO ()
type LogCall    = GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()


getStatus :: StatusCall -> LogCall -> GLuint -> IO String
getStatus getStatus getLog h = do
    alloca $ \lenPtr -> do
        getStatus h gl_INFO_LOG_LENGTH lenPtr
        len <- peek lenPtr
        case len of
            0 -> return ""
            _ -> withCString (replicate (unsafeCoerce len) '\0') $ getErrorString getLog h (unsafeCoerce len)


getErrorString :: LogCall -> GLuint -> GLsizei -> CString -> IO String
getErrorString getLog h len str = do
    let ptr = unsafeCoerce str
    getLog h len nullPtr ptr
    peekCString str


-- | Load the shader source specified by the given file.
--
-- This function implements an #include mechanism, so the given file can
-- refer to other files.
readSource :: FilePath -> IO String
readSource = fmap B.unpack . readSource'


readSource' :: FilePath -> IO B.ByteString
readSource' file = do
    let includeB      = B.pack "#include"
        newLineB      = B.pack "\n"
        isInclude     = ((==) includeB) . B.take 8
        clean         = B.dropWhile (\c -> c == ' ')
        cleanInclude  = B.filter (\c -> c /= '"') . B.dropWhile (\c -> c /= ' ')
        toLines       = B.splitWith (\c -> c == '\n' || c == '\r')
        addNewLine s  = if (B.last s /= '\n') then B.append s newLineB else s
        parse         = fmap B.concat . (fmap . fmap $ flip B.append newLineB) . sequence .
                        fmap (processLine . clean) . toLines
        processLine l =
            if isInclude l
            then readSource' $ B.unpack . clean . cleanInclude $ l
            else return l
    
    contents <- B.readFile file
    
    dir <- getCurrentDirectory
    let dir' = dir ++ "/" ++ (reverse . dropWhile (\c -> c /= '/') . reverse) file
    
    setCurrentDirectory dir'
    code <- parse contents
    setCurrentDirectory dir
    
    return code


-- | Load a 2D vector.
uniformVec2 :: GLint -> Vector2 -> IO ()
uniformVec2 loc v = glUniform2f loc x' y'
    where x' = unsafeCoerce $ V2.x v
          y' = unsafeCoerce $ V2.y v


-- | Load a 3D vector.
uniformVec3 :: GLint -> Vector3 -> IO ()
uniformVec3 loc v = glUniform3f loc x' y' z'
    where x' = unsafeCoerce $ V3.x v
          y' = unsafeCoerce $ V3.y v
          z' = unsafeCoerce $ V3.z v
    

-- | Load a 4D vector.
uniformVec4 :: GLint -> Vector4 -> IO ()
uniformVec4 loc v = glUniform4f loc x' y' z' w'
    where x' = unsafeCoerce $ V4.x v
          y' = unsafeCoerce $ V4.y v
          z' = unsafeCoerce $ V4.z v
          w' = unsafeCoerce $ V4.w v


-- | Load a 3x3 matrix.
uniformMat3 :: GLint -> Matrix3 -> IO ()
uniformMat3 loc mat =
    with mat $ \ptrMat ->
        glUniformMatrix3fv loc 1 (toEnum 0) (unsafeCoerce ptrMat)


-- | Load a 4x4 matrix.
uniformMat4 :: GLint -> Matrix4 -> IO ()
uniformMat4 loc mat =
    with mat $ \ptrMat ->
        glUniformMatrix4fv loc 1 (toEnum 0) (unsafeCoerce ptrMat)


-- | Load a list of floats.
uniformfl :: GLint -> [GLfloat] -> IO ()
uniformfl loc vals = withArray vals $ \ptr ->
    case length vals of
        1 -> glUniform1fv loc 1 ptr
        2 -> glUniform2fv loc 1 ptr
        3 -> glUniform3fv loc 1 ptr
        4 -> glUniform4fv loc 1 ptr


-- | Load a list of integers.
uniformil :: GLint -> [GLint] -> IO ()
uniformil loc vals = withArray vals $ \ptr ->
    case length vals of
        1 -> glUniform1iv loc 1 ptr
        2 -> glUniform2iv loc 1 ptr
        3 -> glUniform3iv loc 1 ptr
        4 -> glUniform4iv loc 1 ptr






--
-- VAOs
--


-- | A vertex array object.
data VAO = VAO
    { getVAO :: GLuint
    , vaoKey :: Resource
    }


instance Eq VAO where
    vao1 == vao2 = getVAO vao1 == getVAO vao2


instance Ord VAO where
    vao1 < vao2 = getVAO vao1 < getVAO vao2


-- | Create a new vao.
newVAO :: Setup VAO
newVAO = do
    h <- setupIO . alloca $ \ptr -> do
        glGenVertexArrays 1 ptr
        peek ptr
    
    rkey <- register $ deleteVAO h
    return $ VAO h rkey


-- | Release the vao.
releaseVAO :: VAO -> Setup ()
releaseVAO = release . vaoKey


-- | Delete the vao.
deleteVAO :: GLuint -> IO ()
deleteVAO vao = Foreign.with vao $ glDeleteVertexArrays 1


-- | Bind the vao.
bindVAO :: VAO -> IO ()
bindVAO = glBindVertexArray . getVAO


-- | Enable the given vertex attribute of the bound vao.
--
-- See also 'bindVAO'.
enableVAOAttrib :: GLuint -> IO ()
enableVAOAttrib = glEnableVertexAttribArray


-- | Bind the bound buffer to the given point.
attribVAOPointer :: GLuint -> GLint -> GLenum -> Bool -> GLsizei -> Int -> IO ()
attribVAOPointer idx ncomp dattype normalise stride off =
    glVertexAttribPointer idx ncomp dattype (unsafeCoerce normalise) stride (unsafeCoerce off)


-- | Draw the bound vao.
drawArrays :: GLenum -> Int -> Int -> IO ()
drawArrays mode first count = glDrawArrays mode (unsafeCoerce first) (unsafeCoerce count)


-- | Draw the bound vao, indexed mode.
drawElements :: GLenum -> Int -> GLenum -> Ptr a -> IO ()
drawElements mode count t idxs = glDrawElements mode (unsafeCoerce count) t idxs






--
-- BUFFER
--


-- | An OpenGL buffer.
data GLBuffer = GLBuffer
    { getBuffer :: GLuint
    , rkey      :: Resource
    }


-- | The type of target buffer.
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


-- | A buffer usage.
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


-- | Create a new buffer.
newBuffer :: Setup GLBuffer
newBuffer = do
    h <- setupIO . alloca $ \ptr -> do
        glGenBuffers 1 ptr
        peek ptr
    
    rkey <- register $ deleteBuffer h
    return $ GLBuffer h rkey


-- | Release the buffer.
releaseBuffer :: GLBuffer -> Setup ()
releaseBuffer = release . rkey


-- | Delete the buffer.
deleteBuffer :: GLuint -> IO ()
deleteBuffer buf = Foreign.with buf $ glDeleteBuffers 1


-- | Bind the buffer.
bindBuffer :: GLBuffer -> TargetBuffer -> IO ()
bindBuffer buf target = glBindBuffer (fromTarget target) $ getBuffer buf


-- | Set the buffer's data.
bufferData :: TargetBuffer -> Int -> Ptr a -> BufferUsage -> IO ()
bufferData target n bufData usage = glBufferData (fromTarget target) (unsafeCoerce n) bufData (fromUsage usage)


-- | Apply the given function the buffer's id.
withGLBuffer :: GLBuffer -> (GLuint -> a) -> a
withGLBuffer buf f = f $ getBuffer buf






--
-- TEXTURE
--

-- | Represents a texture resource.
data Texture = Texture
    { getTex :: GLuint
    , texKey :: Resource
    }


instance Eq Texture where
    t1 == t2 = getTex t1 == getTex t2


instance Ord Texture where
    t1 < t2 = getTex t1 < getTex t2


-- | Create a new texture.
newTexture :: Setup Texture
newTexture = do
    tex <- setupIO . alloca $ \ptr -> do
        glGenTextures 1 ptr
        peek ptr
    
    rkey <- register $ deleteTexture tex
    return $ Texture tex rkey


-- | Release the texture.
releaseTexture :: Texture -> Setup ()
releaseTexture = release . texKey


-- | Delete the texture.
deleteTexture :: GLuint -> IO ()
--deleteTexture tex = with tex $ glDeleteTextures 1
deleteTexture tex = do
    putStrLn $ "Releasing texture " ++ show tex
    with tex $ glDeleteTextures 1


-- | Load the 'Texture' specified by the given file.
loadTextureImage :: FilePath
                 -> GLenum          -- ^ Texture's min filter.
                 -> GLenum          -- ^ Texture's mag filter.
                 -> Setup Texture
loadTextureImage file minFilter magFilter = do
    image <- loadImage file
    tex   <- newTexture
    setupIO $ do
        let w    = width  image
            h    = height image
            pix  = pixels image
            rgb  = fromIntegral . fromEnum $ gl_RGB
        
        bindTexture tex
        loadTextureData gl_TEXTURE_2D 0 rgb w h 0 gl_RGB gl_UNSIGNED_BYTE pix
        texParami gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $= minFilter
        texParami gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $= magFilter
    
    return tex


-- | Bind the texture.
bindTexture :: Texture -> IO ()
bindTexture = glBindTexture gl_TEXTURE_2D . getTex


-- | Load data onto the bound texture.
--
-- See also 'bindTexture'.
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


-- | Set the bound texture's parameter to the given value.
texParami :: GLenum -> GLenum -> SettableStateVar GLenum
texParami target param = makeSettableStateVar $ \val -> glTexParameteri target param $ fromIntegral . fromEnum $ val


-- | Set the bound texture's parameter to the given value.
texParamf :: GLenum -> GLenum -> SettableStateVar Float
texParamf target param = makeSettableStateVar $ \val -> glTexParameterf target param (unsafeCoerce val)


-- | Set the active texture unit.
activeTexture :: SettableStateVar GLenum
activeTexture = makeSettableStateVar glActiveTexture






--
-- ERROR
--


-- | Get the last OpenGL error.
getGLError :: IO (Maybe String)
getGLError = fmap translate glGetError
    where
        translate err
            | err == gl_NO_ERROR          = Nothing
            | err == gl_INVALID_ENUM      = Just "Invalid enum"
            | err == gl_INVALID_VALUE     = Just "Invalid value"
            | err == gl_INVALID_OPERATION = Just "Invalid operation"
            | err == gl_OUT_OF_MEMORY     = Just "Out of memory"
            | otherwise                   = Just "Unknown error"


-- | Print the last OpenGL error.
printGLError :: IO ()
printGLError = getGLError >>= \err -> case err of
    Nothing  -> return ()
    Just str -> hPutStrLn stderr str


-- | Run the given setup action and check for OpenGL errors.
--
-- If an OpenGL error is produced, an exception is thrown containing
-- the given string appended to the string describing the error.
assertGL :: Setup a -> String -> Setup a
assertGL action err = do
    result <- action
    status <- setupIO getGLError
    case status of
        Just str -> setupError $ "OpenGL error raised: " ++ err ++ "; " ++ str
        Nothing  -> return result
