module Spear.Window
(
    -- * Setup
    Dimensions
,   Context
,   WindowTitle
,   FrameCap
,   DisplayBits(..)
,   WindowMode(..)
    -- * Window
,   Window
,   Width
,   Height
,   Init
,   run
,   withWindow
,   events
    -- * Animation
,   Elapsed
,   Dt
,   Step
,   loop
,   GLFW.swapBuffers
    -- * Input
,   InputEvent(..)
,   Key(..)
,   MouseButton(..)
,   MouseProp(..)
,   MousePos
,   MouseDelta
)
where

import Spear.Game
import Spear.Sys.Timer as Timer

import Data.Char (ord)
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.IO.Class
import GHC.Float
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (DisplayBits(..), WindowMode(..))
import qualified Graphics.Rendering.OpenGL as GL

type Width  = Int
type Height = Int

-- | Window dimensions.
type Dimensions = (Width, Height)

-- | A tuple specifying the desired OpenGL context, of the form (Major, Minor).
type Context = (Int, Int)

type WindowTitle = String

type CloseRequest = MVar Bool

-- | A window.
data Window = Window
     { closeRequest :: CloseRequest
     , inputEvents  :: MVar [InputEvent]
     }

-- | Poll the window's events.
events :: MonadIO m => Window -> m [InputEvent]
events wnd = liftIO $ do
       es <- tryTakeMVar (inputEvents wnd) >>= \xs -> case xs of
                         Nothing -> return []
                         Just es -> return es
       putMVar (inputEvents wnd) []
       return es

-- | Game initialiser.
type Init s = Window -> Game () s

run :: MonadIO m => m (Either String a) -> m ()
run r = do
      result <- r
      case result of
           Left err -> liftIO $ putStrLn err
           Right _ -> return ()

withWindow :: MonadIO m
           => Dimensions -> [DisplayBits] -> WindowMode -> Context
           -> Maybe WindowTitle
           -> Init s
           -> (Window -> Game s a)
           -> m (Either String a)
withWindow dim@(w,h) displayBits windowMode glVersion windowTitle init run =
           liftIO $ flip runGame' () $ do
                  glfwInit
                  wnd <- setup dim displayBits windowMode glVersion windowTitle
                  gameState <- init wnd
                  result <- evalSubGame (run wnd) gameState
                  gameIO GLFW.closeWindow
                  gameIO GLFW.terminate
                  return result

setup :: Dimensions -> [DisplayBits] -> WindowMode -> Context -> Maybe WindowTitle
      -> Game s Window
setup (w, h) displayBits windowMode (major, minor) wndTitle = do
        closeRequest <- liftIO newEmptyMVar
        inputEvents  <- liftIO newEmptyMVar
        let onResize' = onResize inputEvents
        let dimensions = GL.Size (fromIntegral w) (fromIntegral h)
        result <- liftIO $ do
               GLFW.openWindowHint GLFW.OpenGLVersionMajor major
               GLFW.openWindowHint GLFW.OpenGLVersionMinor minor
               compat (major, minor)
               GLFW.disableSpecial GLFW.AutoPollEvent
               GLFW.openWindow dimensions (defaultBits displayBits) windowMode
        when (not result) $ gameError "GLFW.openWindow failed"
        liftIO $ do
               GLFW.windowTitle GL.$= case wndTitle of
                                      Nothing    -> "Spear Game Framework"
                                      Just title -> title
               GLFW.windowCloseCallback GL.$= (onWindowClose closeRequest)
               GLFW.windowSizeCallback GL.$= onResize'
               GLFW.keyCallback GL.$= onKey inputEvents
               GLFW.charCallback GL.$= onChar inputEvents
               GLFW.mouseButtonCallback GL.$= onMouseButton inputEvents
               onMouseMove inputEvents >>= (GLFW.mousePosCallback GL.$=)
               onResize' (GL.Size (fromIntegral w) (fromIntegral h))
        return $ Spear.Window.Window closeRequest inputEvents

defaultBits [] = [DisplayRGBBits 8 8 8]
defaultBits xs = xs

compat (major, minor)
       | major >= 3 = GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCompatProfile
       | otherwise  = return ()

glfwInit :: Game s ()
glfwInit = do
         result <- liftIO GLFW.initialize
         case result of
              False -> gameError "GLFW.initialize failed"
              True  -> return ()

-- | Time elapsed since the application started.
type Elapsed = Double

-- | Time elapsed since the last frame.
type Dt = Float

-- | Return true if the application should continue running, false otherwise.
type Step s = Elapsed -> Dt -> Game s (Bool)

-- | Maximum frame rate.
type FrameCap = Int

-- | Run the application's main loop.
loop :: Maybe FrameCap -> Step s -> Window -> Game s ()
loop (Just maxFPS) step wnd = loopCapped maxFPS step wnd
loop Nothing step wnd = do
     timer <- gameIO $ start newTimer
     loop' (closeRequest wnd) timer 0 step
     return ()

loop' :: CloseRequest -> Timer -> Elapsed -> Step s -> Game s ()
loop' closeRequest timer elapsed step = do
      timer' <- gameIO $ tick timer
      let dt       = getDelta timer'
      let elapsed' = elapsed + float2Double dt
      continue <- step elapsed' dt
      close    <- gameIO $ getRequest closeRequest
      when (continue && (not close)) $ loop' closeRequest timer' elapsed' step

loopCapped :: Int -> Step s -> Window -> Game s ()
loopCapped maxFPS step wnd = do
           let ddt = 1.0 / (fromIntegral maxFPS)
               closeReq = closeRequest wnd
           frameTimer   <- gameIO $ start newTimer
           controlTimer <- gameIO $ start newTimer
           loopCapped' closeReq ddt frameTimer controlTimer 0 step
           return ()

loopCapped' :: CloseRequest -> Float -> Timer -> Timer -> Elapsed -> Step s
            -> Game s ()
loopCapped' closeRequest ddt frameTimer controlTimer elapsed step = do
            controlTimer'  <- gameIO $ tick controlTimer
            frameTimer'    <- gameIO $ tick frameTimer
            let dt         = getDelta frameTimer'
            let elapsed'   = elapsed + float2Double dt
            continue       <- step elapsed' dt
            close          <- gameIO $ getRequest closeRequest
            controlTimer'' <- gameIO $ tick controlTimer'
            let dt = getDelta controlTimer''
            when (dt < ddt) $ gameIO $ Timer.sleep (ddt - dt)
            when (continue && (not close)) $
                 loopCapped' closeRequest ddt frameTimer' controlTimer''
                             elapsed' step

getRequest :: MVar Bool -> IO Bool
getRequest mvar = tryTakeMVar mvar >>= \x -> return $ case x of
           Nothing -> False
           Just x  -> x

onWindowClose :: MVar Bool -> GLFW.WindowCloseCallback
onWindowClose closeRequest = putMVar closeRequest True >> return False

onResize :: MVar [InputEvent] -> GLFW.WindowSizeCallback
onResize es (GL.Size w h) = addEvent es $ Resize (fromIntegral w) (fromIntegral h)

onKey :: MVar [InputEvent] -> GLFW.KeyCallback
onKey es key GLFW.Press   = addEvent es $ KeyDown (fromGLFWkey key)
onKey es key GLFW.Release = addEvent es $ KeyUp   (fromGLFWkey key)

onChar :: MVar [InputEvent] -> GLFW.CharCallback
onChar es c GLFW.Press   = addEvent es $ KeyDown (fromGLFWkey (GLFW.CharKey c))
onChar es c GLFW.Release = addEvent es $ KeyUp   (fromGLFWkey (GLFW.CharKey c))

onMouseButton :: MVar [InputEvent] -> GLFW.MouseButtonCallback
onMouseButton es bt GLFW.Press   = addEvent es $ MouseDown (fromGLFWbutton bt)
onMouseButton es bt GLFW.Release = addEvent es $ MouseUp   (fromGLFWbutton bt)

onMouseMove :: MVar [InputEvent] -> IO GLFW.MousePosCallback
onMouseMove es = newEmptyMVar >>= return . flip onMouseMove' es

onMouseMove' :: MVar MousePos -> MVar [InputEvent] -> GLFW.MousePosCallback
onMouseMove' oldPos es (GL.Position x y) = do
             let (x',y') = (fromIntegral x, fromIntegral y)
             (old_x, old_y) <- tryTakeMVar oldPos >>= \x -> case x of
                  Nothing -> return (x',y')
                  Just p  -> return p
             let delta = (x'-old_x, y'-old_y)
             putMVar oldPos (x',y')
             addEvent es $ MouseMove (x',y') delta

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar val = tryTakeMVar mvar >> putMVar mvar val

addEvent :: MVar [a] -> a -> IO ()
addEvent mvar val = tryTakeMVar mvar >>= \xs -> case xs of
         Nothing -> putMVar mvar [val]
         Just es -> putMVar mvar (val:es)

-- Input

data InputEvent
     = Resize Width Height
     | KeyDown Key
     | KeyUp Key
     | MouseDown MouseButton
     | MouseUp MouseButton
     | MouseMove MousePos MouseDelta
     deriving (Eq, Show)

data Key
     = KEY_A | KEY_B | KEY_C | KEY_D | KEY_E | KEY_F | KEY_G | KEY_H
     | KEY_I | KEY_J | KEY_K | KEY_L | KEY_M | KEY_N | KEY_O | KEY_P
     | KEY_Q | KEY_R | KEY_S | KEY_T | KEY_U | KEY_V | KEY_W | KEY_X
     | KEY_Y | KEY_Z | KEY_0 | KEY_1 | KEY_2 | KEY_3 | KEY_4 | KEY_5
     | KEY_6 | KEY_7 | KEY_8 | KEY_9 | KEY_F1 | KEY_F2 | KEY_F3
     | KEY_F4 | KEY_F5 | KEY_F6 | KEY_F7 | KEY_F8 | KEY_F9 | KEY_F10
     | KEY_F11 | KEY_F12 | KEY_ESC | KEY_SPACE | KEY_UP | KEY_DOWN
     | KEY_LEFT | KEY_RIGHT | KEY_UNKNOWN
     deriving (Eq, Enum, Bounded, Show)

data MouseButton = LMB | RMB | MMB
    deriving (Eq, Enum, Bounded, Show)

data MouseProp = MouseX | MouseY | MouseDX | MouseDY | Wheel | WheelDelta
    deriving (Eq, Enum, Bounded, Show)

type MousePos = (Int,Int)
type MouseDelta = (Int,Int)

fromGLFWkey :: GLFW.Key -> Key
fromGLFWkey (GLFW.CharKey 'A') = KEY_A
fromGLFWkey (GLFW.CharKey 'B') = KEY_B
fromGLFWkey (GLFW.CharKey 'C') = KEY_C
fromGLFWkey (GLFW.CharKey 'D') = KEY_D
fromGLFWkey (GLFW.CharKey 'E') = KEY_E
fromGLFWkey (GLFW.CharKey 'F') = KEY_F
fromGLFWkey (GLFW.CharKey 'G') = KEY_G
fromGLFWkey (GLFW.CharKey 'H') = KEY_H
fromGLFWkey (GLFW.CharKey 'I') = KEY_I
fromGLFWkey (GLFW.CharKey 'J') = KEY_J
fromGLFWkey (GLFW.CharKey 'K') = KEY_K
fromGLFWkey (GLFW.CharKey 'L') = KEY_L
fromGLFWkey (GLFW.CharKey 'M') = KEY_M
fromGLFWkey (GLFW.CharKey 'N') = KEY_N
fromGLFWkey (GLFW.CharKey 'O') = KEY_O
fromGLFWkey (GLFW.CharKey 'P') = KEY_P
fromGLFWkey (GLFW.CharKey 'Q') = KEY_Q
fromGLFWkey (GLFW.CharKey 'R') = KEY_R
fromGLFWkey (GLFW.CharKey 'S') = KEY_S
fromGLFWkey (GLFW.CharKey 'T') = KEY_T
fromGLFWkey (GLFW.CharKey 'U') = KEY_U
fromGLFWkey (GLFW.CharKey 'V') = KEY_V
fromGLFWkey (GLFW.CharKey 'W') = KEY_W
fromGLFWkey (GLFW.CharKey 'X') = KEY_X
fromGLFWkey (GLFW.CharKey 'Y') = KEY_Y
fromGLFWkey (GLFW.CharKey 'Z') = KEY_Z
fromGLFWkey (GLFW.CharKey '0') = KEY_0
fromGLFWkey (GLFW.CharKey '1') = KEY_1
fromGLFWkey (GLFW.CharKey '2') = KEY_2
fromGLFWkey (GLFW.CharKey '3') = KEY_3
fromGLFWkey (GLFW.CharKey '4') = KEY_4
fromGLFWkey (GLFW.CharKey '5') = KEY_5
fromGLFWkey (GLFW.CharKey '6') = KEY_6
fromGLFWkey (GLFW.CharKey '7') = KEY_7
fromGLFWkey (GLFW.CharKey '8') = KEY_8
fromGLFWkey (GLFW.CharKey '9') = KEY_9
fromGLFWkey (GLFW.CharKey ' ') = KEY_SPACE
fromGLFWkey (GLFW.SpecialKey GLFW.F1)  = KEY_F1
fromGLFWkey (GLFW.SpecialKey GLFW.F2)  = KEY_F2
fromGLFWkey (GLFW.SpecialKey GLFW.F3)  = KEY_F3
fromGLFWkey (GLFW.SpecialKey GLFW.F4)  = KEY_F4
fromGLFWkey (GLFW.SpecialKey GLFW.F5)  = KEY_F5
fromGLFWkey (GLFW.SpecialKey GLFW.F6)  = KEY_F6
fromGLFWkey (GLFW.SpecialKey GLFW.F7)  = KEY_F7
fromGLFWkey (GLFW.SpecialKey GLFW.F8)  = KEY_F8
fromGLFWkey (GLFW.SpecialKey GLFW.F9)  = KEY_F9
fromGLFWkey (GLFW.SpecialKey GLFW.F10) = KEY_F10
fromGLFWkey (GLFW.SpecialKey GLFW.F11) = KEY_F11
fromGLFWkey (GLFW.SpecialKey GLFW.F12) = KEY_F12
fromGLFWkey (GLFW.SpecialKey GLFW.ESC) = KEY_ESC
fromGLFWkey (GLFW.SpecialKey GLFW.UP)    = KEY_UP
fromGLFWkey (GLFW.SpecialKey GLFW.DOWN)  = KEY_DOWN
fromGLFWkey (GLFW.SpecialKey GLFW.LEFT)  = KEY_LEFT
fromGLFWkey (GLFW.SpecialKey GLFW.RIGHT) = KEY_RIGHT
fromGLFWkey _ = KEY_UNKNOWN

fromGLFWbutton :: GLFW.MouseButton -> MouseButton
fromGLFWbutton GLFW.ButtonLeft   = LMB
fromGLFWbutton GLFW.ButtonRight  = RMB
fromGLFWbutton GLFW.ButtonMiddle = MMB
