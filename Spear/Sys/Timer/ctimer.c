#include "Timer.h"
#include <stdlib.h>

#ifdef __APPLE__
	#include <mach/mach_time.h>
#elif WIN32
	#define WIN32_LEAN_AND_MEAN
	#include <Windows.h>
#else // Linux
	#include <time.h>
	const double NSEC_TO_SEC = 1.0f/1000000000.0f;
	const double SEC_TO_NSEC = 1000000000.0f;
#endif


static double secondsPerCount;


void timer_initialise_subsystem ()
{
#ifdef WIN32
	__int64 countsPerSec;
	QueryPerformanceFrequency((LARGE_INTEGER*)&countsPerSec);
	secondsPerCount = 1.0 / (double)countsPerSec;
#else
	/*struct timespec ts;
	clock_getres(CLOCK_REALTIME, &ts);
	secondsPerCount = (double)ts.tv_sec + ((double)ts.tv_nsec * NSEC_TO_SEC);*/
	secondsPerCount = 1.0f;
#endif
}


timeReading now ()
{
	timeReading t;
	
#ifdef __APPLE__
	t = mach_absolute_time();
#elif WIN32
	QueryPerformanceCounter((LARGE_INTEGER*)&t);
#else
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	t = ts.tv_sec + ((double)ts.tv_nsec * NSEC_TO_SEC);
#endif
	
	return t;
}


void DECLDIR timer_initialise_timer (timer* t)
{
	t->baseTime		= 0;
	t->pausedTime	= 0;
	t->stopTime		= 0;
	t->prevTime		= 0;
	t->curTime		= 0;
	t->deltaTime	= 0;
	t->stopped		= 1;
}


void timer_tick (timer* t)
{
	if (t->stopped)
	{
		t->deltaTime = 0.0;
		return;
	}

	//Get the time on this frame.
	t->curTime = now();

	//Time delta between the current frame and the previous.
	t->deltaTime = (float) ((t->curTime - t->prevTime) * secondsPerCount);

	//Update for next frame.
	t->prevTime = t->curTime;

	// Force nonnegative. The DXSDK's CDXUTTimer mentions that if the
	// processor goes into a power save mode or we get shuffled to
	// another processor, then mDeltaTime can be negative.
	if(t->deltaTime < 0.0)
	{
		t->deltaTime = 0.0;
	}
}


void timer_reset (timer* t)
{
	t->curTime = now();
	t->baseTime = t->curTime;
	t->prevTime = t->curTime;
	t->stopTime = 0;
	t->stopped = 0;
}


void timer_stop (timer* t)
{
	// Don't do anything if we are already stopped.
	if (!t->stopped)
	{
		// Grab the stop time.
		t->stopTime = now();

		// Now we are stopped.
		t->stopped = 1;
	}
}


void timer_start (timer* t)
{
	// Only start if we are stopped.
	if (t->stopped)
	{
		timeReading startTime = now();

		// Accumulate the paused time.
		t->pausedTime = t->pausedTime + startTime - t->stopTime;

		// Make the previous time valid.
		t->prevTime = startTime;

		//Now we are running.
		t->stopTime = 0;
		t->stopped = 0;
	}
}


void timer_sleep (float seconds)
{
#ifdef WIN32
	Sleep((DWORD)(seconds * 1000));
#else
	struct timespec ts;
	ts.tv_sec  = 0;
	ts.tv_nsec = seconds * SEC_TO_NSEC;
	nanosleep(&ts, NULL);
#endif
}


float timer_get_time (timer* t)
{
	// If we are stopped, we do not count the time we have been stopped for.
	if (t->stopped)
	{
		return (float)((t->stopTime - t->baseTime) * secondsPerCount);
	}
	// Otherwise return the time elapsed since the start of the game without counting the time we have been paused for.
	else
	{
		return (float)((t->curTime - t->baseTime - t->pausedTime) * secondsPerCount);
	}
}


float timer_get_delta (timer* t)
{
	return t->deltaTime;
}


char timer_is_running (timer* t)
{
	return !t->stopped;
}
