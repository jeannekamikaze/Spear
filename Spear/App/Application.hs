module Spear.App.Application
(
    -- * Data types
    Dimensions
,   Context
,   SpearWindow
,   Update
,   Size(..)
,   DisplayBits(..)
,   WindowMode(..)
,   WindowSizeCallback
    -- * Setup
,   setup
,   quit
    -- * Main loop
,   run
,   runCapped
    -- * Helpers
,   swapBuffers
,   getParam
)
where

import Spear.Game
import Spear.Sys.Timer as Timer

import Control.Applicative
import Control.Monad (forever, when)
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import System.Exit
import Unsafe.Coerce

-- | Window dimensions.
type Dimensions = (Int, Int)

-- | A tuple specifying the desired OpenGL context, of the form (Major, Minor).
type Context    = (Int, Int)

-- | Represents a window.
newtype SpearWindow = SpearWindow { rkey :: Resource }

instance ResourceClass SpearWindow where
         getResource = rkey

-- | Set up an application 'SpearWindow'.
setup :: Dimensions -> [DisplayBits] -> WindowMode -> Context
      -> WindowSizeCallback -> Game s SpearWindow
setup (w, h) displayBits windowMode (major, minor) onResize' = do
    glfwInit
    gameIO $ do
        openWindowHint OpenGLVersionMajor major
        openWindowHint OpenGLVersionMinor minor
        disableSpecial AutoPollEvent
        
        let dimensions = GL.Size (unsafeCoerce w) (unsafeCoerce h)
        result <- openWindow dimensions displayBits windowMode
        windowTitle $= "Spear Game Framework"
        GL.viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        
        windowSizeCallback $= (onResize onResize')
        onResize' (Size (fromIntegral w) (fromIntegral h))
        
        initialiseTimingSubsystem
    
    rkey <- register quit
    return $ SpearWindow rkey

glfwInit :: Game s ()
glfwInit = do
    result <- gameIO GLFW.initialize
    case result of
        False -> gameError "GLFW.initialize failed"
        True  -> return ()

-- | Close the application's window.
quit :: IO ()
quit = GLFW.terminate

-- | Return true if the application should continue running, false otherwise.
type Update s = Float -> Game s (Bool)

-- | Run the application's main loop.
run :: Update s -> Game s ()
run update = do
    timer <- gameIO $ start newTimer
    run' timer update

run' :: Timer -> Update s -> Game s ()
run' timer update = do
    timer' <- gameIO $ tick timer
    continue <- update $ getDelta timer'
    opened <- gameIO $ getParam Opened
    case continue && opened of
        False -> return ()
        True  -> run' timer' update

-- | Run the application's main loop, with a limit on the frame rate.
runCapped :: Int -> Update s -> Game s ()
runCapped maxFPS update = do
    let ddt = 1.0 / (fromIntegral maxFPS)
    timer <- gameIO $ start newTimer
    runCapped' ddt timer update

runCapped' :: Float -> Timer -> Update s -> Game s ()
runCapped' ddt timer update = do
    timer' <- gameIO $ tick timer
    continue <- update $ getDelta timer'
    opened <- gameIO $ getParam Opened
    case continue && opened of
        False -> return ()
        True  -> do
            t'' <- gameIO $ tick timer'
            let dt = getDelta t''
            when (dt < ddt) $ gameIO $ Timer.sleep (ddt - dt)
            runCapped' ddt timer' update

onResize :: WindowSizeCallback -> Size -> IO ()
onResize callback s@(Size w h) = do
    GL.viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    callback s
