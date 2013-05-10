#include "Timer.h"
#include <stdlib.h>

#ifdef __APPLE__
    #include <mach/mach_time.h>
#elif WIN32
    #define WIN32_LEAN_AND_MEAN
    #include <Windows.h>
#else // Linux
    #include <time.h>
    const double NSEC_TO_SEC = 1.0 / 1000000000.0;
    const double SEC_TO_NSECd = 1000000000.0;
    const timeReading SEC_TO_NSEC = 1000000000;
#endif

static double secondsPerCount;

static void timer_initialise_subsystem ()
{
#ifdef WIN32
    __int64 countsPerSec;
    QueryPerformanceFrequency((LARGE_INTEGER*)&countsPerSec);
    secondsPerCount = 1.0 / (double)countsPerSec;
#else
    struct timespec ts;
    clock_getres(CLOCK_REALTIME, &ts);
    secondsPerCount = (double)ts.tv_sec + ((double)ts.tv_nsec * NSEC_TO_SEC);
#endif
}

static timeReading now ()
{
    timeReading t;
#ifdef __APPLE__
    t = mach_absolute_time();
#elif WIN32
    QueryPerformanceCounter((LARGE_INTEGER*)&t);
#else
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    t = ts.tv_sec*SEC_TO_NSEC + ts.tv_nsec;
#endif
    return t;
}

void timer_init (Timer* timer)
{
    timer_initialise_subsystem();
    timer_reset (timer);
}

void timer_tick (Timer* timer)
{
    if (timer->stopped)
    {
        timer->deltaTime = 0.0;
        return;
    }

    //Get the time on this frame.
    timer->curTime = now();

    //Time delta between the current frame and the previous.
    timer->deltaTime = (float) ((timer->curTime - timer->prevTime) * secondsPerCount);

    //Update for next frame.
    timer->prevTime = timer->curTime;

    // Force nonnegative. The DXSDK's CDXUTTimer mentions that if the
    // processor goes into a power save mode or we get shuffled to
    // another processor, then the delta time can be negative.
    if(timer->deltaTime < 0.0f)
    {
        timer->deltaTime = 0.0f;
    }
}

void timer_reset (Timer* timer)
{
    timeReading n = now();
    timer->baseTime = n;
    timer->stopTime = n;
    timer->prevTime = n;
    timer->curTime = n;
    timer->pausedTime = 0;
    timer->deltaTime = 0.0f;
    timer->stopped = 1;
}

void timer_stop (Timer* timer)
{
    // Don't do anything if we are already stopped.
    if (!timer->stopped)
    {
        // Grab the stop time.
        timer->stopTime = now();

        // Now we are stopped.
        timer->stopped = 1;
    }
}

void timer_start (Timer* timer)
{
    // Only start if we are stopped.
    if (timer->stopped)
    {
        timeReading startTime = now();

        // Accumulate the paused time.
        timer->pausedTime = timer->pausedTime + startTime - timer->stopTime;

        // Make the previous time valid.
        timer->prevTime = startTime;

        //Now we are running.
        timer->stopTime = 0;
        timer->stopped = 0;
    }
}

double timer_get_time (const Timer* timer)
{
    // If we are stopped, we do not count the time we have been stopped for.
    if (timer->stopped)
    {
        return (double)((timer->stopTime - timer->baseTime) * secondsPerCount);
    }
    // Otherwise return the time elapsed since the start but without
    // taking into account the time we have been stopped for.
    else
    {
        return (double)((timer->curTime - timer->baseTime - timer->pausedTime) * secondsPerCount);
    }
}

float timer_get_delta (const Timer* timer)
{
    return timer->deltaTime;
}

char timer_is_running (const Timer* timer)
{
    return !timer->stopped;
}

void timer_sleep (float seconds)
{
#ifdef WIN32
    Sleep((DWORD)(seconds * 1000));
#else
    struct timespec ts;
    ts.tv_sec  = (int) seconds;
    ts.tv_nsec = (long) ((double)(seconds - (int)seconds) * SEC_TO_NSECd);
    nanosleep(&ts, NULL);
#endif
}
