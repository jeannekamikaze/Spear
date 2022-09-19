module Spear.Window
  ( -- * Setup
    Dimensions,
    Context,
    WindowTitle,

    -- * Window
    Window,
    Width,
    Height,
    Init,
    withWindow,
    pollEvents,
    shouldWindowClose,
    swapBuffers,

    -- * Input
    whenKeyDown,
    whenKeyUp,
    processKeys,
    processButtons,
    InputEvent (..),
    Key (..),
    MouseButton (..),
    MouseProp (..),
    MousePos,
    MouseDelta,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (foldM, unless, void, when)
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Graphics.UI.GLFW as GLFW
import Spear.Game

type Width = Int

type Height = Int

-- | Window dimensions.
type Dimensions = (Width, Height)

-- | A pair specifying the desired OpenGL context, of the form (Major, Minor).
type Context = (Int, Int)

type WindowTitle = String

type CloseRequest = MVar Bool

-- | Game initialiser.
type Init s = Window -> Game () s

-- | Window exception.
newtype WindowException = WindowException String deriving (Show)

instance Exception WindowException

-- | A window.
data Window = Window
  { glfwWindow :: GLFW.Window,
    closeRequest :: CloseRequest,
    inputEvents :: MVar [InputEvent]
  }

withWindow ::
  Dimensions ->
  Context ->
  Maybe WindowTitle ->
  Init s ->
  (Window -> Game s a) ->
  IO a
withWindow dim@(w, h) glVersion windowTitle init run = do
  flip runGame' () $ do
    window <- gameIO $ do
      success <- GLFW.init
      unless success $ throw (WindowException "GLFW.initialize failed")
      setup dim glVersion windowTitle
    gameIO $ GLFW.makeContextCurrent (Just . glfwWindow $ window)
    gameState <- init window
    result <- evalSubGame (run window) gameState
    gameIO $ do
      GLFW.destroyWindow $ glfwWindow window
      GLFW.terminate
    return result

setup ::
  Dimensions ->
  Context ->
  Maybe WindowTitle ->
  IO Window
setup (w, h) (major, minor) windowTitle = do
  closeRequest <- newEmptyMVar
  inputEvents <- newEmptyMVar
  let onResize' = onResize inputEvents
  let title = fromMaybe "" windowTitle
  let monitor = Nothing
  maybeWindow <- do
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor major
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor minor
    when (major >= 3) $ GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Compat
    GLFW.createWindow w h title monitor Nothing
  unless (isJust maybeWindow) $ throwIO (WindowException "GLFW.openWindow failed")
  let window = fromJust maybeWindow
  GLFW.setWindowCloseCallback window . Just $ onWindowClose closeRequest
  GLFW.setWindowSizeCallback window . Just $ onResize'
  GLFW.setKeyCallback window . Just $ onKey inputEvents
  GLFW.setCharCallback window . Just $ onChar inputEvents
  GLFW.setMouseButtonCallback window . Just $ onMouseButton inputEvents
  onMouseMove inputEvents >>= GLFW.setCursorPosCallback window . Just
  onResize' window w h
  return $ Spear.Window.Window window closeRequest inputEvents

-- | Poll the window's events.
pollEvents :: Window -> IO [InputEvent]
pollEvents window = do
  GLFW.pollEvents
  events <-
    tryTakeMVar (inputEvents window) >>= \xs -> case xs of
      Nothing -> return []
      Just events -> return events
  putMVar (inputEvents window) []
  return events

-- | Return true when the user requests to close the window.
shouldWindowClose :: Window -> IO Bool
shouldWindowClose = getRequest . closeRequest

-- | Swaps buffers.
swapBuffers :: Window -> IO ()
swapBuffers = GLFW.swapBuffers . glfwWindow

getRequest :: MVar Bool -> IO Bool
getRequest mvar =
  tryTakeMVar mvar >>= \x -> return $ fromMaybe False x

onWindowClose :: MVar Bool -> GLFW.WindowCloseCallback
onWindowClose closeRequest window = do putMVar closeRequest True

onResize :: MVar [InputEvent] -> GLFW.WindowSizeCallback
onResize events window w h = addEvent events $ Resize w h

onKey :: MVar [InputEvent] -> GLFW.KeyCallback
onKey events window key _ GLFW.KeyState'Pressed _ = addEvent events $ KeyDown (fromGLFWkey key)
onKey events window key _ GLFW.KeyState'Released _ = addEvent events $ KeyUp (fromGLFWkey key)
onKey events window key _ GLFW.KeyState'Repeating _ = return ()

onChar :: MVar [InputEvent] -> GLFW.CharCallback
onChar events window char = addEvent events $ KeyDown . fromGLFWkey . read $ [char]

onMouseButton :: MVar [InputEvent] -> GLFW.MouseButtonCallback
onMouseButton events window button GLFW.MouseButtonState'Pressed _ = addEvent events $ MouseDown (fromGLFWbutton button)
onMouseButton events window button GLFW.MouseButtonState'Released _ = addEvent events $ MouseUp (fromGLFWbutton button)

onMouseMove :: MVar [InputEvent] -> IO GLFW.CursorPosCallback
onMouseMove events = newEmptyMVar >>= return . flip onMouseMove' events

onMouseMove' :: MVar MousePos -> MVar [InputEvent] -> GLFW.CursorPosCallback
onMouseMove' oldPos events window x y = do
  (old_x, old_y) <-
    tryTakeMVar oldPos >>= \old -> case old of
      Nothing -> return (x, y)
      Just p -> return p
  let delta = (x - old_x, y - old_y)
  putMVar oldPos (x, y)
  addEvent events $ MouseMove (x, y) delta

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mvar val = tryTakeMVar mvar >> putMVar mvar val

addEvent :: MVar [a] -> a -> IO ()
addEvent mvar val =
  tryTakeMVar mvar >>= \xs -> case xs of
    Nothing -> putMVar mvar [val]
    Just events -> putMVar mvar (val : events)

-- Input

-- | Run the game action when the key is down.
whenKeyDown :: GLFW.Window -> Key -> Game s a -> Game s ()
whenKeyDown = whenKeyInState (== GLFW.KeyState'Pressed)

-- | Run the game action when the key is up.
whenKeyUp :: GLFW.Window -> Key -> Game s a -> Game s ()
whenKeyUp = whenKeyInState (== GLFW.KeyState'Released)

whenKeyInState :: (GLFW.KeyState -> Bool) -> GLFW.Window -> Key -> Game s a -> Game s ()
whenKeyInState pred window key game = do
  isDown <- fmap pred $ gameIO . GLFW.getKey window . toGLFWkey $ key
  when isDown $ void game

-- | Process the keyboard keys, returning those values for which their
-- corresponding key is pressed.
processKeys :: GLFW.Window -> [(Key, a)] -> Game s [a]
processKeys window = foldM f []
  where
    f acc (key, result) = do
      isDown <-
        fmap (== GLFW.KeyState'Pressed) $
          gameIO . GLFW.getKey window . toGLFWkey $ key
      return $ if isDown then result : acc else acc

-- | Process the mouse buttons, returning those values for which their
-- corresponding button is pressed.
processButtons :: GLFW.Window -> [(MouseButton, a)] -> Game s [a]
processButtons window = foldM f []
  where
    f acc (button, result) = do
      isDown <-
        fmap (== GLFW.MouseButtonState'Pressed) $
          gameIO . GLFW.getMouseButton window . toGLFWbutton $ button
      return $ if isDown then result : acc else acc

data InputEvent
  = Resize Width Height
  | KeyDown Key
  | KeyUp Key
  | MouseDown MouseButton
  | MouseUp MouseButton
  | MouseMove MousePos MouseDelta
  deriving (Eq, Show)

data Key
  = KEY_A
  | KEY_B
  | KEY_C
  | KEY_D
  | KEY_E
  | KEY_F
  | KEY_G
  | KEY_H
  | KEY_I
  | KEY_J
  | KEY_K
  | KEY_L
  | KEY_M
  | KEY_N
  | KEY_O
  | KEY_P
  | KEY_Q
  | KEY_R
  | KEY_S
  | KEY_T
  | KEY_U
  | KEY_V
  | KEY_W
  | KEY_X
  | KEY_Y
  | KEY_Z
  | KEY_0
  | KEY_1
  | KEY_2
  | KEY_3
  | KEY_4
  | KEY_5
  | KEY_6
  | KEY_7
  | KEY_8
  | KEY_9
  | KEY_F1
  | KEY_F2
  | KEY_F3
  | KEY_F4
  | KEY_F5
  | KEY_F6
  | KEY_F7
  | KEY_F8
  | KEY_F9
  | KEY_F10
  | KEY_F11
  | KEY_F12
  | KEY_ESC
  | KEY_SPACE
  | KEY_UP
  | KEY_DOWN
  | KEY_LEFT
  | KEY_RIGHT
  | KEY_UNKNOWN
  deriving (Eq, Enum, Bounded, Show)

data MouseButton = LMB | RMB | MMB
  deriving (Eq, Enum, Bounded, Show)

data MouseProp = MouseX | MouseY | MouseDX | MouseDY | Wheel | WheelDelta
  deriving (Eq, Enum, Bounded, Show)

type MousePos = (Double, Double)

type MouseDelta = (Double, Double)

fromGLFWkey :: GLFW.Key -> Key
fromGLFWkey GLFW.Key'A = KEY_A
fromGLFWkey GLFW.Key'B = KEY_B
fromGLFWkey GLFW.Key'C = KEY_C
fromGLFWkey GLFW.Key'D = KEY_D
fromGLFWkey GLFW.Key'E = KEY_E
fromGLFWkey GLFW.Key'F = KEY_F
fromGLFWkey GLFW.Key'G = KEY_G
fromGLFWkey GLFW.Key'H = KEY_H
fromGLFWkey GLFW.Key'I = KEY_I
fromGLFWkey GLFW.Key'J = KEY_J
fromGLFWkey GLFW.Key'K = KEY_K
fromGLFWkey GLFW.Key'L = KEY_L
fromGLFWkey GLFW.Key'M = KEY_M
fromGLFWkey GLFW.Key'N = KEY_N
fromGLFWkey GLFW.Key'O = KEY_O
fromGLFWkey GLFW.Key'P = KEY_P
fromGLFWkey GLFW.Key'Q = KEY_Q
fromGLFWkey GLFW.Key'R = KEY_R
fromGLFWkey GLFW.Key'S = KEY_S
fromGLFWkey GLFW.Key'T = KEY_T
fromGLFWkey GLFW.Key'U = KEY_U
fromGLFWkey GLFW.Key'V = KEY_V
fromGLFWkey GLFW.Key'W = KEY_W
fromGLFWkey GLFW.Key'X = KEY_X
fromGLFWkey GLFW.Key'Y = KEY_Y
fromGLFWkey GLFW.Key'Z = KEY_Z
fromGLFWkey GLFW.Key'0 = KEY_0
fromGLFWkey GLFW.Key'1 = KEY_1
fromGLFWkey GLFW.Key'2 = KEY_2
fromGLFWkey GLFW.Key'3 = KEY_3
fromGLFWkey GLFW.Key'4 = KEY_4
fromGLFWkey GLFW.Key'5 = KEY_5
fromGLFWkey GLFW.Key'6 = KEY_6
fromGLFWkey GLFW.Key'7 = KEY_7
fromGLFWkey GLFW.Key'8 = KEY_8
fromGLFWkey GLFW.Key'9 = KEY_9
fromGLFWkey GLFW.Key'Space = KEY_SPACE
fromGLFWkey GLFW.Key'F1 = KEY_F1
fromGLFWkey GLFW.Key'F2 = KEY_F2
fromGLFWkey GLFW.Key'F3 = KEY_F3
fromGLFWkey GLFW.Key'F4 = KEY_F4
fromGLFWkey GLFW.Key'F5 = KEY_F5
fromGLFWkey GLFW.Key'F6 = KEY_F6
fromGLFWkey GLFW.Key'F7 = KEY_F7
fromGLFWkey GLFW.Key'F8 = KEY_F8
fromGLFWkey GLFW.Key'F9 = KEY_F9
fromGLFWkey GLFW.Key'F10 = KEY_F10
fromGLFWkey GLFW.Key'F11 = KEY_F11
fromGLFWkey GLFW.Key'F12 = KEY_F12
fromGLFWkey GLFW.Key'Escape = KEY_ESC
fromGLFWkey GLFW.Key'Up = KEY_UP
fromGLFWkey GLFW.Key'Down = KEY_DOWN
fromGLFWkey GLFW.Key'Left = KEY_LEFT
fromGLFWkey GLFW.Key'Right = KEY_RIGHT
fromGLFWkey _ = KEY_UNKNOWN

-- https://www.glfw.org/docs/3.3/group__buttons.html
fromGLFWbutton :: GLFW.MouseButton -> MouseButton
fromGLFWbutton GLFW.MouseButton'1 = LMB
fromGLFWbutton GLFW.MouseButton'2 = RMB
fromGLFWbutton GLFW.MouseButton'3 = MMB

toGLFWkey :: Key -> GLFW.Key
toGLFWkey KEY_A = GLFW.Key'A
toGLFWkey KEY_B = GLFW.Key'B
toGLFWkey KEY_C = GLFW.Key'C
toGLFWkey KEY_D = GLFW.Key'D
toGLFWkey KEY_E = GLFW.Key'E
toGLFWkey KEY_F = GLFW.Key'F
toGLFWkey KEY_G = GLFW.Key'G
toGLFWkey KEY_H = GLFW.Key'H
toGLFWkey KEY_I = GLFW.Key'I
toGLFWkey KEY_J = GLFW.Key'J
toGLFWkey KEY_K = GLFW.Key'K
toGLFWkey KEY_L = GLFW.Key'L
toGLFWkey KEY_M = GLFW.Key'M
toGLFWkey KEY_N = GLFW.Key'N
toGLFWkey KEY_O = GLFW.Key'O
toGLFWkey KEY_P = GLFW.Key'P
toGLFWkey KEY_Q = GLFW.Key'Q
toGLFWkey KEY_R = GLFW.Key'R
toGLFWkey KEY_S = GLFW.Key'S
toGLFWkey KEY_T = GLFW.Key'T
toGLFWkey KEY_U = GLFW.Key'U
toGLFWkey KEY_V = GLFW.Key'V
toGLFWkey KEY_W = GLFW.Key'W
toGLFWkey KEY_X = GLFW.Key'X
toGLFWkey KEY_Y = GLFW.Key'Y
toGLFWkey KEY_Z = GLFW.Key'Z
toGLFWkey KEY_0 = GLFW.Key'0
toGLFWkey KEY_1 = GLFW.Key'1
toGLFWkey KEY_2 = GLFW.Key'2
toGLFWkey KEY_3 = GLFW.Key'3
toGLFWkey KEY_4 = GLFW.Key'4
toGLFWkey KEY_5 = GLFW.Key'5
toGLFWkey KEY_6 = GLFW.Key'6
toGLFWkey KEY_7 = GLFW.Key'7
toGLFWkey KEY_8 = GLFW.Key'8
toGLFWkey KEY_9 = GLFW.Key'9
toGLFWkey KEY_SPACE = GLFW.Key'Space
toGLFWkey KEY_F1 = GLFW.Key'F1
toGLFWkey KEY_F2 = GLFW.Key'F2
toGLFWkey KEY_F3 = GLFW.Key'F3
toGLFWkey KEY_F4 = GLFW.Key'F4
toGLFWkey KEY_F5 = GLFW.Key'F5
toGLFWkey KEY_F6 = GLFW.Key'F6
toGLFWkey KEY_F7 = GLFW.Key'F7
toGLFWkey KEY_F8 = GLFW.Key'F8
toGLFWkey KEY_F9 = GLFW.Key'F9
toGLFWkey KEY_F10 = GLFW.Key'F10
toGLFWkey KEY_F11 = GLFW.Key'F11
toGLFWkey KEY_F12 = GLFW.Key'F12
toGLFWkey KEY_ESC = GLFW.Key'Escape
toGLFWkey KEY_UP = GLFW.Key'Up
toGLFWkey KEY_DOWN = GLFW.Key'Down
toGLFWkey KEY_LEFT = GLFW.Key'Left
toGLFWkey KEY_RIGHT = GLFW.Key'Right
toGLFWkey KEY_UNKNOWN = GLFW.Key'Unknown

-- https://www.glfw.org/docs/3.3/group__buttons.html
toGLFWbutton :: MouseButton -> GLFW.MouseButton
toGLFWbutton LMB = GLFW.MouseButton'1
toGLFWbutton RMB = GLFW.MouseButton'2
toGLFWbutton MMB = GLFW.MouseButton'3
