#ifndef _SPEAR_TIMER_H
#define _SPEAR_TIMER_H

#ifdef _MSC_VER
	#ifdef DLL_EXPORT
		#define DECLDIR __declspec(dllexport)
	#else
		#define DECLDIR __declspec(dllimport)
	#endif
#else
	#define DECLDIR
#endif

#ifdef WIN32
	#ifdef _MSC_VER
		typedef __int64 timeReading;
	#else
		typedef __UINT64_TYPE__ timeReading;
	#endif
#else
	typedef double timeReading;
#endif

#ifdef __cplusplus
extern C {
#endif
	
typedef struct
{	
	timeReading baseTime;
	timeReading pausedTime;
	timeReading stopTime;
	timeReading prevTime;
	timeReading curTime;
	float deltaTime;
	char stopped;
} timer;

/// Initialises the timing subsystem.
void DECLDIR timer_initialise_subsystem ();

/// Initialises a timer.
void DECLDIR timer_initialise_timer (timer* t);

/// Call every frame.
void DECLDIR timer_tick (timer* t);

/// Call before message loop.
void DECLDIR timer_reset (timer* t);

/// Call when paused.
void DECLDIR timer_stop (timer* t);

/// Call when unpaused.
void DECLDIR timer_start (timer* t);

/// Puts the caller thread to sleep for the given number of seconds.
void DECLDIR timer_sleep (float seconds);

/// Returns total running time in seconds.
float DECLDIR timer_get_time (timer* t);

/// Returns the elapsed time in seconds.
float DECLDIR timer_get_delta (timer* t);

/// Gets the timer's running state.
char DECLDIR timer_is_running (timer* t);

#ifdef __cplusplus
}
#endif

#endif // _SPEAR_TIMER_H
