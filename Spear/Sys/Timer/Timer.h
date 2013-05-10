#pragma once

#ifdef WIN32
#ifdef _MSC_VER
typedef __int64 timeReading;
#else
typedef __UINT64_TYPE__ timeReading;
#endif
#else
typedef __UINT64_TYPE__  timeReading;
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
  Header: Timer
  A high resolution timer module.
*/

/*
  Struct: Timer
*/
typedef struct
{
    timeReading baseTime;   // The instant since we start timing.
    timeReading stopTime;   // The instant the timer is stopped.
    timeReading prevTime;   // The instant the timer was ticked prior to the last tick.
    timeReading curTime;    // The instant the timer was last ticked.
    timeReading pausedTime; // Amount of time the timer has been stopped for.
    float deltaTime; // Amount of time elapsed since the last call to tick.
    char stopped;
} Timer;

/*
  Function: timer_init
  Construct a new timer.

  The timer is initialised by making a call to reset(). Since time
  calculations are measured from the instant the timer is reset (base time),
  you probably want to make a manual call to reset() at the start of
  your application, otherwise the application will be measuring times
  from the instant the timer's constructor is called, which can be error prone.

  A call to start() must be made prior to any time calculations, as the
  timer is initialised as stopped.
*/
void timer_init (Timer*);

/*
  Function: timer_tick
  Update the timer's values.

  This function updates the timer's running time and caches the time
  elapsed since the last tick or since the start if this is the first
  tick after the last call to start().

  This function has no effect on a stopped ticker.
*/
void timer_tick (Timer*);

/*
  Function: timer_start
  Start the timer.

  This function starts the timer for the first time or resumes it
  after a call to stop().

  Note that this function does not reset the timer's base time;
  it's only a mechanism to resume a stopped timer.
*/
void timer_start (Timer*);

/*
  Function: timer_stop
  Stop the timer.

  This function essentially freezes time; any values dependent on
  the timer will behave as if time had not passed since the moment
  the timer was stopped.

  To resume the timer call start().
*/
void timer_stop (Timer*);

/*
  Function: timer_reset
  Reset the timer.

  This function resets all of the timer's values such as running and
  stop times and sets the timer to stopped. The total running time is
  then measured from the instant the timer is reset, making the timer
  behave as a newly constructed one.

  A call to start() must be made prior to any further time calculations.
*/
void timer_reset (Timer*);

/*
  Function: timer_get_time
  Get the total running time.

  The amount of time the timer has been stopped for is not taken
  into account.
*/
double timer_get_time (const Timer*);

/*
  Function: timer_get_delta
  Get the time elapsed since the last tick, or since the start if
  this is the first tick.
*/
float timer_get_delta (const Timer*);

/*
  Function: timer_is_running
  Return true if the timer is running (not stopped), false otherwise.
*/
char timer_is_running (const Timer*);

/*
  Function: timer_sleep
  Put the caller thread to sleep for the given number of seconds.
*/
void timer_sleep (float seconds);

#ifdef __cplusplus
}
#endif
