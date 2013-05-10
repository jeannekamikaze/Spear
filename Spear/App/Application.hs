module Spear.App.Application
(
    -- * Setup
    Dimensions
,   Context
,   WindowTitle
,   SpearWindow
,   Update
,   Size(..)
,   DisplayBits(..)
,   WindowMode(..)
,   WindowSizeCallback
,   withWindow
    -- * Main loop
,   loop
,   loopCapped
    -- * Helpers
,   swapBuffers
)
where

import Spear.Game
import Spear.Sys.Timer as Timer

import Control.Concurrent.MVar
import Control.Monad (when)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL

-- | Window dimensions.
type Dimensions = (Int, Int)

-- | A tuple specifying the desired OpenGL context, of the form (Major, Minor).
type Context = (Int, Int)

type WindowTitle = String

-- Whether the user has closed the window.
type CloseRequested = MVar Bool

-- | Represents a window.
data SpearWindow = SpearWindow
     { closeRequest :: CloseRequested
     }

withWindow :: Dimensions -> [DisplayBits] -> WindowMode -> Context -> Maybe WindowTitle
           -> WindowSizeCallback -> (SpearWindow -> Game s a) -> Game s a
withWindow dim displayBits windowMode glVersion windowTitle onResize run = do
           glfwInit
           window <- setup dim displayBits windowMode glVersion windowTitle onResize
           gs <- getGameState
           (a,s) <- runSubGame (run window) gs
           gameIO GLFW.closeWindow
           gameIO GLFW.terminate
           saveGameState s
           return a

-- Set up an application 'SpearWindow'.
setup :: Dimensions -> [DisplayBits] -> WindowMode -> Context -> Maybe WindowTitle
      -> WindowSizeCallback -> Game s SpearWindow
setup (w, h) displayBits windowMode (major, minor) wndTitle onResize' = do
    closeRequest <- gameIO $ newEmptyMVar
    gameIO $ do
        openWindowHint OpenGLVersionMajor major
        openWindowHint OpenGLVersionMinor minor
        openWindowHint OpenGLProfile OpenGLCompatProfile
        disableSpecial AutoPollEvent

        let dimensions = GL.Size (fromIntegral w) (fromIntegral h)
        result <- openWindow dimensions displayBits windowMode
        windowTitle $= case wndTitle of
                    Nothing    -> "Spear Game Framework"
                    Just title -> title
        GL.viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

        windowSizeCallback $= (onResize onResize')
        windowCloseCallback $= (onWindowClose closeRequest)
        onResize' (Size (fromIntegral w) (fromIntegral h))

    return $ SpearWindow closeRequest

glfwInit :: Game s ()
glfwInit = do
    result <- gameIO GLFW.initialize
    case result of
        False -> gameError "GLFW.initialize failed"
        True  -> return ()

-- | Return true if the application should continue running, false otherwise.
type Update s = Float -> Game s (Bool)

-- | Run the application's main loop.
loop :: SpearWindow -> Update s -> Game s ()
loop wnd update = do
     gs <- getGameState
     flip runSubGame gs $ do
            timer <- gameIO $ start newTimer
            run (closeRequest wnd) timer update
     return ()

run :: CloseRequested -> Timer -> Update s -> Game s ()
run closeRequest timer update = do
    timer'   <- gameIO $ tick timer
    continue <- update $ getDelta timer'
    close    <- gameIO $ getRequest closeRequest
    when (continue && (not close)) $ run closeRequest timer' update

-- | Run the application's main loop with a limit on the frame rate.
loopCapped :: SpearWindow -> Int -> Update s -> Game s ()
loopCapped wnd maxFPS update = do
    gs <- getGameState
    flip runSubGame gs $ do
           let ddt = 1.0 / (fromIntegral maxFPS)
               closeReq = closeRequest wnd
           frameTimer   <- gameIO $ start newTimer
           controlTimer <- gameIO $ start newTimer
           runCapped closeReq ddt frameTimer controlTimer update
    return ()

runCapped :: CloseRequested -> Float -> Timer -> Timer -> Update s -> Game s ()
runCapped closeRequest ddt frameTimer controlTimer update = do
    controlTimer'  <- gameIO $ tick controlTimer
    frameTimer'    <- gameIO $ tick frameTimer
    continue       <- update $ getDelta frameTimer'
    close          <- gameIO $ getRequest closeRequest
    controlTimer'' <- gameIO $ tick controlTimer'
    let dt = getDelta controlTimer''
    when (dt < ddt) $ gameIO $ Timer.sleep (ddt - dt)
    when (continue && (not close)) $
         runCapped closeRequest ddt frameTimer' controlTimer'' update

getRequest :: MVar Bool -> IO Bool
getRequest mvar = tryTakeMVar mvar >>= \x -> return $ case x of
           Nothing -> False
           Just x  -> x

onResize :: WindowSizeCallback -> Size -> IO ()
onResize callback s@(Size w h) = do
    GL.viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    callback s

onWindowClose :: MVar Bool -> WindowCloseCallback
onWindowClose closeRequest = putMVar closeRequest True >> return False
