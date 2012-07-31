module Spear.GLSL.Management
(
    -- * Data types
    GLSLShader
,   GLSLProgram
,   ShaderType(..)
    -- * Program manipulation
,   newProgram
,   releaseProgram
,   linkProgram
,   useProgram
,   withGLSLProgram
    -- * Shader manipulation
,   attachShader
,   detachShader
,   loadShader
,   newShader
,   releaseShader
    -- ** Source loading
,   loadSource
,   shaderSource
,   readSource
,   compile
    -- * Location
,   attribLocation
,   fragLocation
,   uniformLocation
    -- * Helper functions
,   ($=)
,   Data.StateVar.get
)
where


import Spear.Setup

import Control.Monad ((<=<), forM)
import Control.Monad.Trans.State as State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import Control.Monad (mapM_, when)
import qualified Data.ByteString.Char8 as B
import Data.StateVar
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Graphics.Rendering.OpenGL.Raw.Core31
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory)
import Unsafe.Coerce


-- | Represents a GLSL shader handle.
data GLSLShader  = GLSLShader
    { getShader    :: GLuint
    , getShaderKey :: Resource
    }


-- | Represents a GLSL program handle.
data GLSLProgram = GLSLProgram
    { getProgram    :: GLuint
    , getProgramKey :: Resource
    }
    
    
-- | Encodes several shader types.
data ShaderType = VertexShader | FragmentShader deriving (Eq, Show)


toGLShader :: ShaderType -> GLenum
toGLShader VertexShader   = gl_VERTEX_SHADER
toGLShader FragmentShader = gl_FRAGMENT_SHADER


-- | Apply the given function to the GLSLProgram's id.
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


-- | Create a 'GLSLProgram'.
newProgram :: [GLSLShader] -> Setup GLSLProgram
newProgram shaders = do
    h <- setupIO glCreateProgram
    when (h == 0) $ setupError "glCreateProgram failed"
    rkey <- register $ deleteProgram h
    let program = GLSLProgram h rkey
    
    mapM_ (setupIO . attachShader program) shaders
    linkProgram program
    
    return program


-- | Release the given 'GLSLProgram'.
releaseProgram :: GLSLProgram -> Setup ()
releaseProgram = release . getProgramKey


-- | Delete the given 'GLSLProgram'.
deleteProgram :: GLuint -> IO ()
--deleteProgram = glDeleteProgram
deleteProgram prog = do
    putStrLn $ "Deleting shader program " ++ show prog
    glDeleteProgram prog


-- | Link the given GLSL program.
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


-- | Use the given GLSL program.
useProgram :: GLSLProgram -> IO ()
useProgram prog = glUseProgram $ getProgram prog


-- | Attach the given GLSL shader to the given GLSL program.
attachShader :: GLSLProgram -> GLSLShader -> IO ()
attachShader prog shader = glAttachShader (getProgram prog) (getShader shader)


-- | Detach the given GLSL shader from the given GLSL program.
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


-- | Release the given 'GLSLShader'.
releaseShader :: GLSLShader -> Setup ()
releaseShader = release . getShaderKey


-- | Free the given shader.
deleteShader :: GLuint -> IO ()
--deleteShader = glDeleteShader
deleteShader shader = do
    putStrLn $ "Deleting shader " ++ show shader
    glDeleteShader shader


-- | Load a shader source from the file specified by the given string into the given shader.
loadSource :: FilePath -> GLSLShader -> Setup ()
loadSource file h = do
    exists <- setupIO $ doesFileExist file
    case exists of
        False -> setupError "the specified shader file does not exist"
        True  -> setupIO $ do
            code <- readSource file
            withCString code $ shaderSource h


-- | Load the given shader source into the given shader.
shaderSource :: GLSLShader -> CString -> IO ()
shaderSource shader str =
    let ptr = unsafeCoerce str
    in  withArray [ptr] $ flip (glShaderSource (getShader shader) 1) nullPtr


-- | Compile the given shader.
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

