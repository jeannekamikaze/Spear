{-# INCLUDE "Timer/Timer.h" #-}
{-# LINE 1 "Timer.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
{-# LINE 2 "Timer.hsc" #-}
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


import Foreign
import Foreign.C.Types
import Control.Monad
import System.IO.Unsafe



{-# LINE 28 "Timer.hsc" #-}
type TimeReading = CDouble

{-# LINE 30 "Timer.hsc" #-}

data Timer = Timer {
    getBaseTime     :: TimeReading
,   getPausedTime   :: TimeReading
,   getStopTime     :: TimeReading
,   getPrevTime     :: TimeReading
,   getCurTime      :: TimeReading
,   getDeltaTime    :: CFloat
,   getRunning      :: CChar
}



{-# LINE 43 "Timer.hsc" #-}


instance Storable Timer where
    sizeOf _    = (48)
{-# LINE 47 "Timer.hsc" #-}
	alignment _	= alignment (undefined :: TimeReading)
	
	peek ptr = do
		baseTime <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 51 "Timer.hsc" #-}
		pausedTime <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 52 "Timer.hsc" #-}
		stopTime <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 53 "Timer.hsc" #-}
		prevTime <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 54 "Timer.hsc" #-}
		curTime <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 55 "Timer.hsc" #-}
		deltaTime <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 56 "Timer.hsc" #-}
		stopped <- (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 57 "Timer.hsc" #-}
		return $ Timer baseTime pausedTime stopTime prevTime curTime deltaTime stopped
		
	poke ptr (Timer baseTime pausedTime stopTime prevTime curTime deltaTime stopped) = do
		(\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr baseTime
{-# LINE 61 "Timer.hsc" #-}
		(\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr pausedTime
{-# LINE 62 "Timer.hsc" #-}
		(\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr stopTime
{-# LINE 63 "Timer.hsc" #-}
		(\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr prevTime
{-# LINE 64 "Timer.hsc" #-}
		(\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr curTime
{-# LINE 65 "Timer.hsc" #-}
		(\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr deltaTime
{-# LINE 66 "Timer.hsc" #-}
		(\hsc_ptr -> pokeByteOff hsc_ptr 44) ptr stopped
{-# LINE 67 "Timer.hsc" #-}


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
