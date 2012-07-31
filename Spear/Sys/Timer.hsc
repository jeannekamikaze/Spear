{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
module Spear.Sys.Timer
(
	Timer
,	initialiseTimingSubsystem
,	newTimer
,	tick
,	reset
,	stop
,	start
,	sleep
,	getTime
,	getDelta
,	isRunning
)
where


import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Control.Monad
import System.IO.Unsafe


#ifdef WIN32
type TimeReading = CULLong
#else
type TimeReading = CDouble
#endif

data Timer = Timer {
	getBaseTime		:: TimeReading
,	getPausedTime	:: TimeReading
,	getStopTime		:: TimeReading
,	getPrevTime		:: TimeReading
,	getCurTime		:: TimeReading
,	getDeltaTime	:: CFloat
,	getRunning		:: CChar
}


#include "Timer/Timer.h"


instance Storable Timer where
	sizeOf _	= #{size timer}
	alignment _	= alignment (undefined :: TimeReading)
	
	peek ptr = do
		baseTime <- #{peek timer, baseTime} ptr
		pausedTime <- #{peek timer, pausedTime} ptr
		stopTime <- #{peek timer, stopTime} ptr
		prevTime <- #{peek timer, prevTime} ptr
		curTime <- #{peek timer, curTime} ptr
		deltaTime <- #{peek timer, deltaTime} ptr
		stopped <- #{peek timer, stopped} ptr
		return $ Timer baseTime pausedTime stopTime prevTime curTime deltaTime stopped
		
	poke ptr (Timer baseTime pausedTime stopTime prevTime curTime deltaTime stopped) = do
		#{poke timer, baseTime} ptr baseTime
		#{poke timer, pausedTime} ptr pausedTime
		#{poke timer, stopTime} ptr stopTime
		#{poke timer, prevTime} ptr prevTime
		#{poke timer, curTime} ptr curTime
		#{poke timer, deltaTime} ptr deltaTime
		#{poke timer, stopped} ptr stopped


foreign import ccall "Timer.h timer_initialise_subsystem"
	c_timer_initialise_subsystem :: IO ()
	
foreign import ccall "Timer.h timer_initialise_timer"
	c_timer_initialise_timer :: Ptr Timer -> IO ()

foreign import ccall "Timer.h timer_tick"
	c_timer_tick :: Ptr Timer -> IO ()

foreign import ccall "Timer.h timer_reset"
	c_timer_reset :: Ptr Timer -> IO ()

foreign import ccall "Timer.h timer_stop"
	c_timer_stop :: Ptr Timer -> IO ()

foreign import ccall "Timer.h timer_start"
	c_timer_start :: Ptr Timer -> IO ()

foreign import ccall "Timer.h timer_sleep"
	c_timer_sleep :: CFloat -> IO ()

foreign import ccall "Timer.h timer_get_time"
	c_timer_get_time :: Ptr Timer -> IO (CFloat)

foreign import ccall "Timer.h timer_get_delta"
	c_timer_get_delta :: Ptr Timer -> IO (CFloat)

foreign import ccall "Timer.h timer_is_running"
	c_timer_is_running :: Ptr Timer -> IO (CChar)


-- | Initialises the timing subsystem.
initialiseTimingSubsystem :: IO ()
initialiseTimingSubsystem = c_timer_initialise_subsystem


-- | Creates a timer.
newTimer :: Timer
newTimer = unsafePerformIO . alloca $ \tptr -> do
	c_timer_initialise_timer tptr
	t <- peek tptr
	return t


-- | Updates the timer.
tick :: Timer -> IO (Timer)
tick t = alloca $ \tptr -> do
	poke tptr t
	c_timer_tick tptr
	t' <- peek tptr
	return t'


-- | Resets the timer.
reset :: Timer -> IO (Timer)
reset t = alloca $ \tptr -> do
	poke tptr t
	c_timer_reset tptr
	t' <- peek tptr
	return t'


-- | Stops the timer.
stop :: Timer -> IO (Timer)
stop t = alloca $ \tptr -> do
	poke tptr t
	c_timer_stop tptr
	t' <- peek tptr
	return t'


-- | Starts the timer.
start :: Timer -> IO (Timer)
start t = alloca $ \tptr -> do
	poke tptr t
	c_timer_start tptr
	t' <- peek tptr
	return t'


-- | Puts the caller thread to sleep for the given number of seconds.
sleep :: Float -> IO ()
sleep = c_timer_sleep . realToFrac


-- | Gets the timer's total running time.
getTime :: Timer -> Float
getTime t = unsafePerformIO . alloca $ \tptr -> do
	poke tptr t
	time <- c_timer_get_time tptr
	return (realToFrac time)


-- | Gets the timer's delta since the last tick.
getDelta :: Timer -> Float
getDelta t = unsafePerformIO . alloca $ \tptr -> do
	poke tptr t
	dt <- c_timer_get_delta tptr
	return (realToFrac dt)


-- | Returns true if the timer is running, false otherwise.
isRunning :: Timer -> Bool
isRunning t = unsafePerformIO . alloca $ \tptr -> do
	poke tptr t
	running <- c_timer_is_running tptr
	return (running /= 0)
