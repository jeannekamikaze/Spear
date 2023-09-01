{-# LANGUAGE FlexibleInstances #-}

module Spear.GL
  ( -- * Programs
    GLSLProgram,
    newProgram,
    linkProgram,
    useProgram,
    unuseProgram,
    withGLSLProgram,

    -- ** Locations
    attribLocation,
    fragLocation,
    uniformLocation,

    -- ** Uniforms
    Uniform (..),

    -- * Shaders
    GLSLShader,
    ShaderType (..),
    attachShader,
    detachShader,
    loadShader,
    newShader,

    -- ** Source loading
    loadSource,
    shaderSource,
    readSource,
    compile,

    -- * Helper functions
    ($=),
    Data.StateVar.get,

    -- * VAOs
    VAO,
    newVAO,
    bindVAO,
    unbindVAO,
    enableVAOAttrib,
    attribVAOPointer,

    -- ** Rendering
    drawArrays,
    drawElements,

    -- * Buffers
    GLBuffer,
    TargetBuffer (..),
    BufferUsage (..),
    newBuffer,
    bindBuffer,
    unbindBuffer,
    BufferData (..),
    bufferData',
    withGLBuffer,

    -- * Textures
    Texture,
    SettableStateVar,
    ($),

    -- ** Creation and destruction
    newTexture,
    loadTextureImage,

    -- ** Manipulation
    bindTexture,
    unbindTexture,
    loadTextureData,
    texParami,
    texParamf,
    activeTexture,

    -- * Error Handling
    getGLError,
    printGLError,
    assertGL,

    -- * OpenGL
    module Graphics.GL.Core46,
    Ptr,
    nullPtr,
  )
where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8     as B
import           Data.StateVar
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc     (alloca)
import           Foreign.Marshal.Array     (withArray)
import           Foreign.Marshal.Utils     as Foreign (with)
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable          (peek)
import           Graphics.GL.Core46
import           Prelude                   hiding ((*))
import           Spear.Assets.Image
import           Spear.Game
import           Spear.Math.Algebra
import           Spear.Math.Matrix3        (Matrix3)
import           Spear.Math.Matrix4        (Matrix4)
import           Spear.Math.Vector
import           System.Directory          (doesFileExist, getCurrentDirectory,
                                            setCurrentDirectory)
import           System.IO                 (hPutStrLn, stderr)
import           Unsafe.Coerce

--
-- MANAGEMENT
--

-- | A GLSL shader handle.
data GLSLShader = GLSLShader
  { getShader    :: GLuint,
    getShaderKey :: Resource
  }

instance ResourceClass GLSLShader where
  getResource = getShaderKey

-- | A GLSL program handle.
data GLSLProgram = GLSLProgram
  { getProgram    :: GLuint,
    getProgramKey :: Resource
  }

instance ResourceClass GLSLProgram where
  getResource = getProgramKey

-- | Supported shader types.
data ShaderType = VertexShader | FragmentShader | GeometryShader deriving (Eq, Show)

toGLShader :: ShaderType -> GLenum
toGLShader VertexShader   = GL_VERTEX_SHADER
toGLShader FragmentShader = GL_FRAGMENT_SHADER
toGLShader GeometryShader = GL_GEOMETRY_SHADER

-- | Apply the given function to the program's id.
withGLSLProgram :: GLSLProgram -> (GLuint -> a) -> a
withGLSLProgram prog f = f $ getProgram prog

-- | Get the location of the given uniform variable within the given program.
uniformLocation :: GLSLProgram -> String -> GettableStateVar GLint
uniformLocation prog var = makeGettableStateVar $
  withCString var $ \str -> glGetUniformLocation (getProgram prog) (unsafeCoerce str)

-- | Get or set the location of the given variable to a fragment shader colour number.
fragLocation :: GLSLProgram -> String -> StateVar GLint
fragLocation prog var = makeStateVar get set
  where
    get = withCString var $ \str -> glGetFragDataLocation (getProgram prog) (unsafeCoerce str)
    set idx = withCString var $ \str ->
      glBindFragDataLocation (getProgram prog) (unsafeCoerce idx) (unsafeCoerce str)

-- | Get or set the location of the given attribute within the given program.
attribLocation :: GLSLProgram -> String -> StateVar GLint
attribLocation prog var = makeStateVar get set
  where
    get = withCString var $ \str -> glGetAttribLocation (getProgram prog) (unsafeCoerce str)
    set idx = withCString var $ \str ->
      glBindAttribLocation (getProgram prog) (unsafeCoerce idx) (unsafeCoerce str)

-- | Create a new program.
newProgram :: [GLSLShader] -> Game s GLSLProgram
newProgram shaders = do
  h <- gameIO glCreateProgram
  when (h == 0) $ gameError "glCreateProgram failed"
  rkey <- register $ deleteProgram h
  let program = GLSLProgram h rkey
  mapM_ (gameIO . attachShader program) shaders
  linkProgram program
  return program

--  Delete the program.
deleteProgram :: GLuint -> IO ()
--deleteProgram = glDeleteProgram
deleteProgram prog = do
  putStrLn $ "Deleting shader program " ++ show prog
  glDeleteProgram prog

-- | Link the program.
linkProgram :: GLSLProgram -> Game s ()
linkProgram prog = do
  let h = getProgram prog
  err <- gameIO $ do
    glLinkProgram h
    alloca $ \statptr -> do
      glGetProgramiv h GL_LINK_STATUS statptr
      status <- peek statptr
      case status of
        0 -> getStatus glGetProgramiv glGetProgramInfoLog h
        _ -> return ""

  case length err of
    0 -> return ()
    _ -> gameError err

-- | Use the program.
useProgram :: GLSLProgram -> IO ()
useProgram prog = glUseProgram $ getProgram prog

-- | Deactivate the active program.
unuseProgram :: IO ()
unuseProgram = glUseProgram 0

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
loadShader :: ShaderType -> FilePath -> Game s GLSLShader
loadShader shaderType file = do
  shader <- newShader shaderType
  loadSource file shader
  compile file shader
  return shader

-- | Create a new shader.
newShader :: ShaderType -> Game s GLSLShader
newShader shaderType = do
  h <- gameIO $ glCreateShader (toGLShader shaderType)
  case h of
    0 -> gameError "glCreateShader failed"
    _ -> do
      rkey <- register $ deleteShader h
      return $ GLSLShader h rkey

-- | Free the shader.
deleteShader :: GLuint -> IO ()
--deleteShader = glDeleteShader
deleteShader shader = do
  putStrLn $ "Deleting shader " ++ show shader
  glDeleteShader shader

-- | Load a shader source from the file specified by the given string
-- into the shader.
loadSource :: FilePath -> GLSLShader -> Game s ()
loadSource file h = do
  exists <- gameIO $ doesFileExist file
  case exists of
    False -> gameError "the specified shader file does not exist"
    True -> gameIO $ do
      code <- readSource file
      withCString code $ shaderSource h

-- | Load the given shader source into the shader.
shaderSource :: GLSLShader -> CString -> IO ()
shaderSource shader str =
  let ptr = unsafeCoerce str
   in withArray [ptr] $ flip (glShaderSource (getShader shader) 1) nullPtr

-- | Compile the shader.
compile :: FilePath -> GLSLShader -> Game s ()
compile file shader = do
  let h = getShader shader

  -- Compile
  gameIO $ glCompileShader h

  -- Verify status
  err <- gameIO $
    alloca $ \statusPtr -> do
      glGetShaderiv h GL_COMPILE_STATUS statusPtr
      result <- peek statusPtr
      case result of
        0 -> getStatus glGetShaderiv glGetShaderInfoLog h
        _ -> return ""

  case length err of
    0 -> return ()
    _ -> gameError $ "Unable to compile shader " ++ file ++ ":\n" ++ err

type StatusCall = GLuint -> GLenum -> Ptr GLint -> IO ()

type LogCall = GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()

getStatus :: StatusCall -> LogCall -> GLuint -> IO String
getStatus getStatus getLog h = do
  alloca $ \lenPtr -> do
    getStatus h GL_INFO_LOG_LENGTH lenPtr
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
  let includeB = B.pack "#include"
      newLineB = B.pack "\n"
      isInclude = ((==) includeB) . B.take 8
      clean = B.dropWhile (\c -> c == ' ')
      cleanInclude = B.filter (\c -> c /= '"') . B.dropWhile (\c -> c /= ' ')
      toLines = B.splitWith (\c -> c == '\n' || c == '\r')
      addNewLine s = if (B.last s /= '\n') then B.append s newLineB else s
      parse =
        fmap B.concat . (fmap . fmap $ flip B.append newLineB) . sequence
          . fmap (processLine . clean)
          . toLines
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

class Uniform a where
  -- | Load a list of uniform values.
  uniform :: GLint -> a -> IO ()

instance Uniform Int where uniform loc a = glUniform1i loc (fromIntegral a)

instance Uniform Float where uniform loc a = glUniform1f loc a

instance Uniform CFloat where uniform loc a = glUniform1f loc (unsafeCoerce a)

instance Uniform (Int, Int) where
  uniform loc (x, y) = glUniform2i loc (fromIntegral x) (fromIntegral y)

instance Uniform (Float, Float) where
  uniform loc (x, y) = glUniform2f loc x y

instance Uniform (Int, Int, Int) where
  uniform loc (x, y, z) = glUniform3i loc (fromIntegral x) (fromIntegral y) (fromIntegral z)

instance Uniform (Float, Float, Float) where
  uniform loc (x, y, z) = glUniform3f loc x y z

instance Uniform (Int, Int, Int, Int) where
  uniform loc (x, y, z, w) =
    glUniform4i
      loc
      (fromIntegral x)
      (fromIntegral y)
      (fromIntegral z)
      (fromIntegral w)

instance Uniform (Float, Float, Float, Float) where
  uniform loc (x, y, z, w) = glUniform4f loc x y z w

instance Uniform Vector2 where
  uniform loc v = glUniform2f loc x' y'
    where
      x' = unsafeCoerce $ x v
      y' = unsafeCoerce $ y v

instance Uniform Vector3 where
  uniform loc v = glUniform3f loc x' y' z'
    where
      x' = unsafeCoerce $ x v
      y' = unsafeCoerce $ y v
      z' = unsafeCoerce $ z v

instance Uniform Vector4 where
  uniform loc v = glUniform4f loc x' y' z' w'
    where
      x' = unsafeCoerce $ x v
      y' = unsafeCoerce $ y v
      z' = unsafeCoerce $ z v
      w' = unsafeCoerce $ w v

instance Uniform Matrix3 where
  uniform loc mat =
    with mat $ \ptrMat ->
      glUniformMatrix3fv loc 1 (toEnum 0) (unsafeCoerce ptrMat)

instance Uniform Matrix4 where
  uniform loc mat =
    with mat $ \ptrMat ->
      glUniformMatrix4fv loc 1 (toEnum 0) (unsafeCoerce ptrMat)

instance Uniform [Float] where
  uniform loc vals = withArray (map unsafeCoerce vals) $ \ptr ->
    case length vals of
      1 -> glUniform1fv loc 1 ptr
      2 -> glUniform2fv loc 1 ptr
      3 -> glUniform3fv loc 1 ptr
      4 -> glUniform4fv loc 1 ptr

instance Uniform [CFloat] where
  uniform loc vals = withArray vals $ \ptr ->
    case length vals of
      1 -> glUniform1fv loc 1 $ castPtr ptr
      2 -> glUniform2fv loc 1 $ castPtr ptr
      3 -> glUniform3fv loc 1 $ castPtr ptr
      4 -> glUniform4fv loc 1 $ castPtr ptr

instance Uniform [Int] where
  uniform loc vals = withArray (map fromIntegral vals) $ \ptr ->
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
  { getVAO :: GLuint,
    vaoKey :: Resource
  }

instance ResourceClass VAO where
  getResource = vaoKey

instance Eq VAO where
  vao1 == vao2 = getVAO vao1 == getVAO vao2

instance Ord VAO where
  vao1 < vao2 = getVAO vao1 < getVAO vao2
  vao1 <= vao2 = getVAO vao1 <= getVAO vao2

-- | Create a new vao.
newVAO :: Game s VAO
newVAO = do
  h <- gameIO . alloca $ \ptr -> do
    glGenVertexArrays 1 ptr
    peek ptr

  rkey <- register $ deleteVAO h
  return $ VAO h rkey

-- | Delete the vao.
deleteVAO :: GLuint -> IO ()
deleteVAO vao = Foreign.with vao $ glDeleteVertexArrays 1

-- | Bind the vao.
bindVAO :: VAO -> IO ()
bindVAO = glBindVertexArray . getVAO

-- | Unbind the bound vao.
unbindVAO :: IO ()
unbindVAO = glBindVertexArray 0

-- | Enable the given vertex attribute of the bound vao.
--
-- See also 'bindVAO'.
enableVAOAttrib ::
  -- | Attribute index.
  GLuint ->
  IO ()
enableVAOAttrib = glEnableVertexAttribArray

-- | Bind the bound buffer to the given point.
attribVAOPointer ::
  -- | The index of the generic vertex attribute to be modified.
  GLuint ->
  -- | The number of components per generic vertex attribute. Must be 1,2,3,4.
  GLint ->
  -- | The data type of each component in the array.
  GLenum ->
  -- | Whether fixed-point data values should be normalized.
  Bool ->
  -- | Stride. Byte offset between consecutive generic vertex attributes.
  GLsizei ->
  -- | Offset to the first component in the array.
  Int ->
  IO ()
attribVAOPointer idx ncomp dattype normalise stride off =
  glVertexAttribPointer idx ncomp dattype normalise' stride (unsafeCoerce off)
  where
    normalise' = if normalise then 1 else 0

-- | Draw the bound vao.
drawArrays ::
  -- | The kind of primitives to render.
  GLenum ->
  -- | Starting index in the enabled arrays.
  Int ->
  -- | The number of indices to be rendered.
  Int ->
  IO ()
drawArrays mode first count = glDrawArrays mode (unsafeCoerce first) (unsafeCoerce count)

-- | Draw the bound vao, indexed mode.
drawElements ::
  -- | The kind of primitives to render.
  GLenum ->
  -- | The number of elements to be rendered.
  Int ->
  -- | The type of the index values. Must be one of gl_UNSIGNED_BYTE, gl_UNSIGNED_SHORT, or gl_UNSIGNED_INT.
  GLenum ->
  -- | Pointer to the location where indices are stored, or offset into the index array when there is a bound ElementArrayBuffer.
  Ptr a ->
  IO ()
drawElements mode count t idxs = glDrawElements mode (unsafeCoerce count) t idxs

--
-- BUFFER
--

-- | An OpenGL buffer.
data GLBuffer = GLBuffer
  { getBuffer :: GLuint,
    rkey      :: Resource
  }

instance ResourceClass GLBuffer where
  getResource = rkey

-- | The type of target buffer.
data TargetBuffer
  = ArrayBuffer
  | ElementArrayBuffer
  | PixelPackBuffer
  | PixelUnpackBuffer
  deriving (Eq, Show)

fromTarget :: TargetBuffer -> GLenum
fromTarget ArrayBuffer        = GL_ARRAY_BUFFER
fromTarget ElementArrayBuffer = GL_ELEMENT_ARRAY_BUFFER
fromTarget PixelPackBuffer    = GL_PIXEL_PACK_BUFFER
fromTarget PixelUnpackBuffer  = GL_PIXEL_UNPACK_BUFFER

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
fromUsage StreamDraw  = GL_STREAM_DRAW
fromUsage StreamRead  = GL_STREAM_READ
fromUsage StreamCopy  = GL_STREAM_COPY
fromUsage StaticDraw  = GL_STATIC_DRAW
fromUsage StaticRead  = GL_STATIC_READ
fromUsage StaticCopy  = GL_STATIC_COPY
fromUsage DynamicDraw = GL_DYNAMIC_DRAW
fromUsage DynamicRead = GL_DYNAMIC_READ
fromUsage DynamicCopy = GL_DYNAMIC_COPY

-- | Create a new buffer.
newBuffer :: Game s GLBuffer
newBuffer = do
  h <- gameIO . alloca $ \ptr -> do
    glGenBuffers 1 ptr
    peek ptr

  rkey <- register $ deleteBuffer h
  return $ GLBuffer h rkey

-- | Delete the buffer.
deleteBuffer :: GLuint -> IO ()
deleteBuffer buf = Foreign.with buf $ glDeleteBuffers 1

-- | Bind the buffer.
bindBuffer :: TargetBuffer -> GLBuffer -> IO ()
bindBuffer target buf = glBindBuffer (fromTarget target) $ getBuffer buf

-- | Unbind the bound buffer.
unbindBuffer :: TargetBuffer -> IO ()
unbindBuffer target = glBindBuffer (fromTarget target) 0

class Storable a => BufferData a where
  -- | Set the buffer's data.
  bufferData :: TargetBuffer -> [a] -> BufferUsage -> IO ()
  bufferData tgt vals usage =
    let n = sizeOf (head vals) * length vals
     in withArray vals $ \ptr -> bufferData' tgt n ptr usage

instance BufferData Word8

instance BufferData Word16

instance BufferData Word32

instance BufferData CChar

instance BufferData CInt

instance BufferData CFloat

instance BufferData CDouble

instance BufferData Int

instance BufferData Float

instance BufferData Double

{-bufferData :: Storable a
           => TargetBuffer
           -> Int -- ^ The size in bytes of an element in the data list.
           -> [a] -- ^ The data list.
           -> BufferUsage
           -> IO ()
bufferData target n bufData usage = withArray bufData $
    \ptr -> bufferData target (n * length bufData) ptr usage-}

-- | Set the buffer's data.
bufferData' ::
  TargetBuffer ->
  -- | Buffer size in bytes.
  Int ->
  Ptr a ->
  BufferUsage ->
  IO ()
bufferData' target n bufData usage = glBufferData (fromTarget target) (unsafeCoerce n) bufData (fromUsage usage)

-- | Apply the given function the buffer's id.
withGLBuffer :: GLBuffer -> (GLuint -> a) -> a
withGLBuffer buf f = f $ getBuffer buf

--
-- TEXTURE
--

-- | Represents a texture resource.
data Texture = Texture
  { getTex :: GLuint,
    texKey :: Resource
  }

instance Eq Texture where
  t1 == t2 = getTex t1 == getTex t2

instance Ord Texture where
  t1 < t2 = getTex t1 < getTex t2
  t1 <= t2 = getTex t1 <= getTex t2

instance ResourceClass Texture where
  getResource = texKey

-- | Create a new texture.
newTexture :: Game s Texture
newTexture = do
  tex <- gameIO . alloca $ \ptr -> do
    glGenTextures 1 ptr
    peek ptr

  rkey <- register $ deleteTexture tex
  return $ Texture tex rkey

-- | Delete the texture.
deleteTexture :: GLuint -> IO ()
--deleteTexture tex = with tex $ glDeleteTextures 1
deleteTexture tex = do
  putStrLn $ "Releasing texture " ++ show tex
  with tex $ glDeleteTextures 1

-- | Load the 'Texture' specified by the given file.
loadTextureImage ::
  FilePath ->
  -- | Texture's min filter.
  GLenum ->
  -- | Texture's mag filter.
  GLenum ->
  Game s Texture
loadTextureImage file minFilter magFilter = do
  image <- loadImage file
  tex <- newTexture
  gameIO $ do
    let w = width image
        h = height image
        pix = pixels image
        rgb = fromIntegral . fromEnum $ GL_RGB

    bindTexture tex
    loadTextureData GL_TEXTURE_2D 0 rgb w h 0 GL_RGB GL_UNSIGNED_BYTE pix
    texParami GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER $= minFilter
    texParami GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER $= magFilter

  return tex

-- | Bind the texture.
bindTexture :: Texture -> IO ()
bindTexture = glBindTexture GL_TEXTURE_2D . getTex

-- | Unbind the bound texture.
unbindTexture :: IO ()
unbindTexture = glBindTexture GL_TEXTURE_2D 0

-- | Load data onto the bound texture.
--
-- See also 'bindTexture'.
loadTextureData ::
  GLenum ->
  -- | Target
  Int ->
  -- | Level
  Int ->
  -- | Internal format
  Int ->
  -- | Width
  Int ->
  -- | Height
  Int ->
  -- | Border
  GLenum ->
  -- | Texture type
  GLenum ->
  -- | Texture data
  Ptr a ->
  IO ()
loadTextureData target level internalFormat width height border format texType texData = do
  glTexImage2D
    target
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
      | err == GL_NO_ERROR = Nothing
      | err == GL_INVALID_ENUM = Just "Invalid enum"
      | err == GL_INVALID_VALUE = Just "Invalid value"
      | err == GL_INVALID_OPERATION = Just "Invalid operation"
      | err == GL_OUT_OF_MEMORY = Just "Out of memory"
      | otherwise = Just "Unknown error"

-- | Print the last OpenGL error.
printGLError :: IO ()
printGLError =
  getGLError >>= \err -> case err of
    Nothing  -> return ()
    Just str -> hPutStrLn stderr str

-- | Run the given setup action and check for OpenGL errors.
--
-- If an OpenGL error is produced, an exception is thrown containing
-- the given string appended to the string describing the error.
assertGL :: Game s a -> String -> Game s a
assertGL action err = do
  result <- action
  status <- gameIO getGLError
  case status of
    Just str -> gameError $ "OpenGL error raised: " ++ err ++ "; " ++ str
    Nothing  -> return result
