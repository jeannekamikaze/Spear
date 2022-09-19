module Spear.App
  ( Elapsed,
    Dt,
    Step,
    loop,
  )
where

import Control.Monad
import GHC.Float
import Spear.Game
import Spear.Sys.Timer as Timer
import Spear.Window

maxFPS = 60

-- | Time elapsed since the application started.
type Elapsed = Double

-- | Time elapsed since the last frame.
type Dt = Float

-- | Return true if the application should continue running, false otherwise.
type Step s = Elapsed -> Dt -> [InputEvent] -> Game s Bool

-- | Enter the main application loop.
loop :: Step s -> Window -> Game s ()
loop step window = do
  let ddt = 1.0 / fromIntegral maxFPS -- Desired delta time.
  frameTimer <- gameIO $ start newTimer
  controlTimer <- gameIO $ start newTimer
  loop' window ddt frameTimer controlTimer 0 step
  return ()

loop' ::
  Window ->
  Dt ->
  Timer ->
  Timer ->
  Elapsed ->
  Step s ->
  Game s ()
loop' window ddt frameTimer controlTimer elapsed step = do
  controlTimer' <- gameIO $ tick controlTimer
  frameTimer' <- gameIO $ tick frameTimer
  let dt = getDelta frameTimer'
  let elapsed' = elapsed + float2Double dt
  inputEvents <- gameIO $ pollEvents window
  continue <- step elapsed' dt inputEvents
  gameIO $ swapBuffers window
  close <- gameIO $ shouldWindowClose window
  controlTimer'' <- gameIO $ tick controlTimer'
  let dt = getDelta controlTimer''
  when (dt < ddt) $ gameIO $ Timer.sleep (ddt - dt)
  when (continue && not close) $
    loop'
      window
      ddt
      frameTimer'
      controlTimer''
      elapsed'
      step
