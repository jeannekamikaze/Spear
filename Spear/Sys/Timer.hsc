{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
module Spear.Sys.Timer
(
    Timer
,   newTimer
,   tick
,   start
,   stop
,   reset
,   getTime
,   getDelta
,   isRunning
,   sleep
)
where

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable
import Control.Monad
import System.IO.Unsafe

#ifdef WIN32
type TimeReading = CULLong
#else
type TimeReading = CDouble
#endif

data Timer = Timer
     { getBaseTime   :: TimeReading
     , getPausedTime :: TimeReading
     , getStopTime   :: TimeReading
     , getPrevTime   :: TimeReading
     , getCurTime    :: TimeReading
     , getDeltaTime  :: CFloat
     , getRunning    :: CChar
     }

#include "Timer/Timer.h"

instance Storable Timer where
         sizeOf _    = #{size Timer}
         alignment _ = alignment (undefined :: TimeReading)

         peek ptr = do
              baseTime   <- #{peek Timer, baseTime}   ptr
	      pausedTime <- #{peek Timer, pausedTime} ptr
              stopTime   <- #{peek Timer, stopTime}   ptr
              prevTime   <- #{peek Timer, prevTime}   ptr
              curTime    <- #{peek Timer, curTime}    ptr
              deltaTime  <- #{peek Timer, deltaTime}  ptr
              stopped    <- #{peek Timer, stopped}    ptr
              return $ Timer baseTime pausedTime stopTime prevTime curTime deltaTime stopped

         poke ptr (Timer baseTime pausedTime stopTime prevTime curTime deltaTime stopped) = do
              #{poke Timer, baseTime}   ptr baseTime
              #{poke Timer, pausedTime} ptr pausedTime
              #{poke Timer, stopTime}   ptr stopTime
              #{poke Timer, prevTime}   ptr prevTime
              #{poke Timer, curTime}    ptr curTime
              #{poke Timer, deltaTime}  ptr deltaTime
              #{poke Timer, stopped}    ptr stopped

foreign import ccall unsafe "Timer.h timer_init"
	c_timer_init :: Ptr Timer -> IO ()

foreign import ccall unsafe "Timer.h timer_tick"
	c_timer_tick :: Ptr Timer -> IO ()

foreign import ccall unsafe "Timer.h timer_start"
	c_timer_start :: Ptr Timer -> IO ()

foreign import ccall unsafe "Timer.h timer_stop"
	c_timer_stop :: Ptr Timer -> IO ()

foreign import ccall unsafe "Timer.h timer_reset"
	c_timer_reset :: Ptr Timer -> IO ()

foreign import ccall unsafe "Timer.h timer_get_time"
	c_timer_get_time :: Ptr Timer -> IO (CDouble)

foreign import ccall unsafe "Timer.h timer_get_delta"
	c_timer_get_delta :: Ptr Timer -> IO (CFloat)

foreign import ccall unsafe "Timer.h timer_is_running"
	c_timer_is_running :: Ptr Timer -> IO (CChar)

foreign import ccall "Timer.h timer_sleep"
	c_timer_sleep :: CFloat -> IO ()

-- | Construct a new timer.
newTimer :: Timer
newTimer = unsafePerformIO . unsafeInterleaveIO . alloca $ \tptr -> do
         c_timer_init tptr
         peek tptr

-- | Update the timer.
tick :: Timer -> IO (Timer)
tick t = alloca $ \tptr -> do
     poke tptr t
     c_timer_tick tptr
     peek tptr

-- | Start the timer.
start :: Timer -> IO (Timer)
start t = alloca $ \tptr -> do
	poke tptr t
	c_timer_start tptr
	t' <- peek tptr
	return t'

-- | Stop the timer.
stop :: Timer -> IO (Timer)
stop t = alloca $ \tptr -> do
     poke tptr t
     c_timer_stop tptr
     peek tptr

-- | Reset the timer.
reset :: Timer -> IO (Timer)
reset t = alloca $ \tptr -> do
      poke tptr t
      c_timer_reset tptr
      peek tptr

-- | Get the timer's total running time.
getTime :: Timer -> Double
getTime t = unsafeDupablePerformIO . alloca $ \tptr -> do
        poke tptr t
        time <- c_timer_get_time tptr
        return (realToFrac time)

-- | Get the time elapsed between the last two ticks.
getDelta :: Timer -> Float
getDelta t = unsafeDupablePerformIO . alloca $ \tptr -> do
         poke tptr t
         dt <- c_timer_get_delta tptr
         return (realToFrac dt)

-- | Return true if the timer is running (not stopped), false otherwise.
isRunning :: Timer -> Bool
isRunning t = unsafeDupablePerformIO . alloca $ \tptr -> do
          poke tptr t
          running <- c_timer_is_running tptr
          return (running /= 0)

-- | Put the caller thread to sleep for the given number of seconds.
sleep :: Float -> IO ()
sleep = c_timer_sleep . realToFrac
