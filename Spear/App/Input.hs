module Spear.App.Input
(
    -- * Data types
    Key(..)
,   MouseButton(..)
,   MouseProp(..)
,   Keyboard
,   Mouse(..)
,   Input(..)
,   ButtonDelay
,   DelayedMouse
    -- * Input state querying
,   newKeyboard
,   getKeyboard
,   newMouse
,   getMouse
,   newInput
,   getInput
,   pollInput
    -- * Toggled input
,   toggledMouse
,   toggledKeyboard
    -- * Delayed input
,   newDM
,   updateDM
,   delayedMouse
    -- * Input modifiers
,   setMousePosition
,   setMouseWheel
)
where

import Data.Char (ord)
import qualified Data.Vector.Unboxed as V
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.StateVar

data Key
    = KEY_A | KEY_B | KEY_C | KEY_D | KEY_E | KEY_F | KEY_G | KEY_H
    | KEY_I | KEY_J | KEY_K | KEY_L | KEY_M | KEY_N | KEY_O | KEY_P
    | KEY_Q | KEY_R | KEY_S | KEY_T | KEY_U | KEY_V | KEY_W | KEY_X
    | KEY_Y | KEY_Z | KEY_0 | KEY_1 | KEY_2 | KEY_3 | KEY_4 | KEY_5
    | KEY_6 | KEY_7 | KEY_8 | KEY_9 | KEY_F1 | KEY_F2 | KEY_F3
    | KEY_F4 | KEY_F5 | KEY_F6 | KEY_F7 | KEY_F8 | KEY_F9 | KEY_F10
    | KEY_F11 | KEY_F12 | KEY_ESC | KEY_SPACE | KEY_UP | KEY_DOWN
    | KEY_LEFT | KEY_RIGHT
    deriving (Enum, Bounded)

type Keyboard = Key -> Bool

data MouseButton = LMB | RMB | MMB
    deriving (Enum, Bounded)

data MouseProp = MouseX | MouseY | MouseDX | MouseDY | Wheel | WheelDelta
    deriving Enum

data Mouse = Mouse
    { button   :: MouseButton -> Bool
    , property :: MouseProp -> Float
    }

data Input = Input
    { keyboard :: Keyboard
    , mouse    :: Mouse
    }

-- | Return a new dummy keyboard.
--
-- This function should be called to get an initial keyboard.
--
-- The returned keyboard has all of its keys unpressed.
--
-- For further keyboard updates, see 'getKeyboard'.
newKeyboard :: Keyboard
newKeyboard = const False

-- | Get the keyboard.
getKeyboard :: IO Keyboard
getKeyboard =
    let keyboard' :: V.Vector Bool -> Keyboard
        keyboard' keystate key = keystate V.! fromEnum key
        keys = fmap toEnum [0..fromEnum (maxBound :: Key)]
    in
        (fmap (V.fromList . fmap ((==) GLFW.Press)) . mapM GLFW.getKey . fmap toGLFWkey $ keys)
            >>= return . keyboard'

-- | Return a new dummy mouse.
--
-- This function should be called to get an initial mouse.
--
-- The returned mouse has all keys unpressed, position set to (0,0) and 0 deta values.
--
-- For further mouse updates, see 'getMouse'.
newMouse :: Mouse
newMouse = Mouse (const False) (const 0)

-- | Get the mouse.
--
-- The previous mouse state is required to compute position deltas.
getMouse :: Mouse -> IO Mouse
getMouse oldMouse =
    let getButton :: V.Vector Bool -> MouseButton -> Bool
        getButton mousestate button = mousestate V.! fromEnum button

        getProp :: V.Vector Float -> MouseProp -> Float
        getProp props prop = props V.! fromEnum prop

        props xpos ypos wheel = V.fromList
            [ xpos
            , ypos
            , xpos - property oldMouse MouseX
            , ypos - property oldMouse MouseY
            , wheel
            , wheel - property oldMouse Wheel
            ]

        getButtonState =
            fmap (V.fromList . fmap ((==) GLFW.Press)) .
            mapM GLFW.getMouseButton .
            fmap toGLFWbutton $ buttons

        buttons = fmap toEnum [0..fromEnum (maxBound :: MouseButton)]
    in do
        Position xpos ypos <- get GLFW.mousePos
        wheel <- get GLFW.mouseWheel
        buttonState <- getButtonState
        return $ Mouse
            { button   = getButton buttonState
            , property = getProp $ props (fromIntegral xpos) (fromIntegral ypos) (fromIntegral wheel)
            }

-- | Return a new dummy input.
newInput :: Input
newInput = Input newKeyboard newMouse

-- | Get input devices.
getInput :: Input -> IO Input
getInput (Input _ oldMouse) = do
    keyboard <- getKeyboard
    mouse    <- getMouse oldMouse
    return $ Input keyboard mouse

-- | Poll input devices.
pollInput :: IO ()
pollInput = GLFW.pollEvents

-- | Return a mouse that reacts to button toggles.
toggledMouse :: Mouse -- ^ Previous mouse state.
             -> Mouse -- ^ Current mouse state.
             -> Mouse -- ^ Toggled mouse.

toggledMouse prev cur = cur { button = \bt -> button cur bt && not (button prev bt) }

-- | Return a keyboard that reacts to key toggles.
toggledKeyboard :: Keyboard -- ^ Previous keyboard state.
                -> Keyboard -- ^ Current keyboard state.
                -> Keyboard -- ^ Toggled keyboard.

toggledKeyboard prev cur key = cur key && not (prev key)

-- | Delay configuration for each mouse button.
type ButtonDelay = MouseButton -> Float


-- | Accumulated delays for each mouse button.
data DelayedMouse = DelayedMouse
    { delayedMouse :: Mouse
    , delay :: ButtonDelay
    , accum :: V.Vector Float
    }

newDM :: ButtonDelay -- ^ Delay configuration for each button.
      -> DelayedMouse
newDM delay = DelayedMouse newMouse delay $
    V.replicate (fromEnum (maxBound :: MouseButton)) 0

updateDM :: DelayedMouse -- ^ Current mouse state.
         -> Float -- ^ Time elapsed since last udpate.
         -> DelayedMouse

updateDM (DelayedMouse mouse delay accum) dt =
    let
        time b     = dt + accum' V.! fromEnum b
        active b   = time b >= delay b
        button' b  = active b && button mouse b
        accum'     = accum V.// fmap newDelay [0 .. fromEnum (maxBound :: MouseButton)]
        newDelay x = let b = toEnum x
                     in (x, if button' b then 0 else time b)
    in
        DelayedMouse mouse { button = button' } delay accum'

-- | Set the mouse position.
setMousePosition :: Integral a => (a,a) -> Mouse -> IO Mouse
setMousePosition (x,y) mouse = do
                 GLFW.mousePos $= Position (fromIntegral x) (fromIntegral y)
                 getMouse mouse

-- | Set the mouse wheel.
setMouseWheel :: Integral a => a -> Mouse -> IO Mouse
setMouseWheel w mouse = do
              GLFW.mouseWheel $= (fromIntegral w)
              getMouse mouse

toGLFWkey :: Key -> Int
toGLFWkey KEY_A   = ord 'A'
toGLFWkey KEY_B   = ord 'B'
toGLFWkey KEY_C   = ord 'C'
toGLFWkey KEY_D   = ord 'D'
toGLFWkey KEY_E   = ord 'E'
toGLFWkey KEY_F   = ord 'F'
toGLFWkey KEY_G   = ord 'G'
toGLFWkey KEY_H   = ord 'H'
toGLFWkey KEY_I   = ord 'I'
toGLFWkey KEY_J   = ord 'J'
toGLFWkey KEY_K   = ord 'K'
toGLFWkey KEY_L   = ord 'L'
toGLFWkey KEY_M   = ord 'M'
toGLFWkey KEY_N   = ord 'N'
toGLFWkey KEY_O   = ord 'O'
toGLFWkey KEY_P   = ord 'P'
toGLFWkey KEY_Q   = ord 'Q'
toGLFWkey KEY_R   = ord 'R'
toGLFWkey KEY_S   = ord 'S'
toGLFWkey KEY_T   = ord 'T'
toGLFWkey KEY_U   = ord 'U'
toGLFWkey KEY_V   = ord 'V'
toGLFWkey KEY_W   = ord 'W'
toGLFWkey KEY_X   = ord 'X'
toGLFWkey KEY_Y   = ord 'Y'
toGLFWkey KEY_Z   = ord 'Z'
toGLFWkey KEY_0   = ord '0'
toGLFWkey KEY_1   = ord '1'
toGLFWkey KEY_2   = ord '2'
toGLFWkey KEY_3   = ord '3'
toGLFWkey KEY_4   = ord '4'
toGLFWkey KEY_5   = ord '5'
toGLFWkey KEY_6   = ord '6'
toGLFWkey KEY_7   = ord '7'
toGLFWkey KEY_8   = ord '8'
toGLFWkey KEY_9   = ord '9'
toGLFWkey KEY_F1  = fromEnum GLFW.F1
toGLFWkey KEY_F2  = fromEnum GLFW.F2
toGLFWkey KEY_F3  = fromEnum GLFW.F3
toGLFWkey KEY_F4  = fromEnum GLFW.F4
toGLFWkey KEY_F5  = fromEnum GLFW.F5
toGLFWkey KEY_F6  = fromEnum GLFW.F6
toGLFWkey KEY_F7  = fromEnum GLFW.F7
toGLFWkey KEY_F8  = fromEnum GLFW.F8
toGLFWkey KEY_F9  = fromEnum GLFW.F9
toGLFWkey KEY_F10 = fromEnum GLFW.F10
toGLFWkey KEY_F11 = fromEnum GLFW.F11
toGLFWkey KEY_F12 = fromEnum GLFW.F12
toGLFWkey KEY_ESC = fromEnum GLFW.ESC
toGLFWkey KEY_SPACE = ord ' '
toGLFWkey KEY_UP    = fromEnum GLFW.UP
toGLFWkey KEY_DOWN  = fromEnum GLFW.DOWN
toGLFWkey KEY_LEFT  = fromEnum GLFW.LEFT
toGLFWkey KEY_RIGHT = fromEnum GLFW.RIGHT


toGLFWbutton :: MouseButton -> GLFW.MouseButton
toGLFWbutton LMB = GLFW.ButtonLeft
toGLFWbutton RMB = GLFW.ButtonRight
toGLFWbutton MMB = GLFW.ButtonMiddle
