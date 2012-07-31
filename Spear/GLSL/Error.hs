module Spear.GLSL.Error
(
    getGLError
,   printGLError
,   assertGL
)
where


import Spear.Setup

import Graphics.Rendering.OpenGL.Raw.Core31
import System.IO (hPutStrLn, stderr)


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


-- | Run the given 'Setup' action and check for OpenGL errors.
-- If an OpenGL error is produced, an exception is thrown
-- containing the given string and the OpenGL error.
assertGL :: Setup a -> String -> Setup a
assertGL action err = do
    result <- action
    status <- setupIO getGLError
    case status of
        Just str -> setupError $ "OpenGL error raised: " ++ err ++ "; " ++ str
        Nothing  -> return result
