---
layout: tutorial_page 
title: "Waiting and Signaling on Condition Variables"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---
## Routines:
    pthread_cond_wait (condition,mutex)
    pthread_cond_signal (condition)
    
    pthread_cond_broadcast (condition)

## Usage:

- pthread_cond_wait() blocks the calling thread until the specified condition is signalled. This routine should be called while mutex is locked, and it will automatically release the mutex while it waits. After signal is received and thread is awakened, mutex will be automatically locked for use by the thread. The programmer is then responsible for unlocking mutex when the thread is finished with it.
- Recommendation: Using a WHILE loop instead of an IF statement (see watch_count routine in example below) to check the waited for condition can help deal with several potential problems, such as:

  - If several threads are waiting for the same wake up signal, they will take turns acquiring the mutex, and any one of them can then modify the condition they all waited for.
  - If the thread received the signal in error due to a program bug
  - The Pthreads library is permitted to issue spurious wake ups to a waiting thread without violating the standard.

- The pthread_cond_signal() routine is used to signal (or wake up) another thread which is waiting on the condition variable. It should be called after mutex is locked, and must unlock mutex in order for pthread_cond_wait() routine to complete.
- The pthread_cond_broadcast() routine should be used instead of pthread_cond_signal() if more than one thread is in a blocking wait state.
- It is a logical error to call pthread_cond_signal() before calling pthread_cond_wait().
- Proper locking and unlocking of the associated mutex variable is essential when using these routines. For example:
  - Failing to lock the mutex before calling pthread_cond_wait() may cause it NOT to block.
  - Failing to unlock the mutex after calling pthread_cond_signal() may not allow a matching pthread_cond_wait() routine to complete (it will remain blocked).

## Example: Using Condition Variables
- This simple example code demonstrates the use of several Pthread condition variable routines.
- The main routine creates three threads.
- Two of the threads perform work and update a "count" variable.
- The third thread waits until the count variable reaches a specified value.

<PRE>
 <FONT Color=#DF4442>#include &LT;pthread.h&GT;</FONT>
 #include &LT;stdio.h&GT;
 #include &LT;stdlib.h&GT;

 #define NUM_THREADS  3
 #define TCOUNT 10
 #define COUNT_LIMIT 12

 int     count = 0;
 int     thread_ids[3] = {0,1,2};
 <FONT Color=#DF4442>pthread_mutex_t count_mutex;</FONT>
 <FONT Color=#DF4442>pthread_cond_t count_threshold_cv;</FONT>

 void *inc_count(void *t) 
 {
   int i;
   long my_id = (long)t;

   for (i=0; i&LT;TCOUNT; i++) {
     <FONT Color=#DF4442>pthread_mutex_lock(&count_mutex);</FONT>
     count++;

     <FONT COLOR=#AAAAAA>/* 
     Check the value of count and signal waiting thread when condition is
     reached.  Note that this occurs while mutex is locked. 
     */</FONT>
     if (count == COUNT_LIMIT) {
       <FONT Color=#DF4442>pthread_cond_signal(&count_threshold_cv);</FONT>
       printf("inc_count(): thread %ld, count = %d  Threshold reached.\n", 
              my_id, count);
       }
     printf("inc_count(): thread %ld, count = %d, unlocking mutex\n", 
	    my_id, count);
     <FONT Color=#DF4442>pthread_mutex_unlock(&count_mutex);</FONT>

     <FONT COLOR=#AAAAAA>/* Do some "work" so threads can alternate on mutex lock */</FONT>
     sleep(1);
     }
   <FONT Color=#DF4442>pthread_exit(NULL);</FONT>
 }

 void *watch_count(void *t) 
 {
   long my_id = (long)t;

   printf("Starting watch_count(): thread %ld\n", my_id);

   <FONT COLOR=#AAAAAA>/*
   Lock mutex and wait for signal.  Note that the pthread_cond_wait 
   routine will automatically and atomically unlock mutex while it waits. 
   Also, note that if COUNT_LIMIT is reached before this routine is run by
   the waiting thread, the loop will be skipped to prevent pthread_cond_wait
   from never returning. 
   */</FONT>
   <FONT Color=#DF4442>pthread_mutex_lock(&count_mutex);</FONT>
   while (count&LT;COUNT_LIMIT) {
     <FONT Color=#DF4442>pthread_cond_wait(&count_threshold_cv, &count_mutex);</FONT>
     printf("watch_count(): thread %ld Condition signal received.\n", my_id);
     }
     count += 125;
     printf("watch_count(): thread %ld count now = %d.\n", my_id, count);
   <FONT Color=#DF4442>pthread_mutex_unlock(&count_mutex);</FONT>
   <FONT Color=#DF4442>pthread_exit(NULL);</FONT>
 }

 int main (int argc, char *argv[])
 {
   int i, rc;
   long t1=1, t2=2, t3=3;
   <FONT Color=#DF4442>pthread_t threads[3];</FONT>
   <FONT Color=#DF4442>pthread_attr_t attr;</FONT>

   <FONT COLOR=#AAAAAA>/* Initialize mutex and condition variable objects */</FONT>
   <FONT Color=#DF4442>pthread_mutex_init(&count_mutex, NULL);</FONT>
   <FONT Color=#DF4442>pthread_cond_init (&count_threshold_cv, NULL);</FONT>

   <FONT COLOR=#AAAAAA>/* For portability, explicitly create threads in a joinable state */</FONT>
   <FONT Color=#DF4442>pthread_attr_init(&attr);</FONT>
   <FONT Color=#DF4442>pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);</FONT>
   <FONT Color=#DF4442>pthread_create(&threads[0], &attr, watch_count, (void *)t1);</FONT>
   <FONT Color=#DF4442>pthread_create(&threads[1], &attr, inc_count, (void *)t2);</FONT>
   <FONT Color=#DF4442>pthread_create(&threads[2], &attr, inc_count, (void *)t3);</FONT>

   <FONT COLOR=#AAAAAA>/* Wait for all threads to complete */</FONT>
   for (i=0; i&LT;NUM_THREADS; i++) {
     <FONT Color=#DF4442>pthread_join(threads[i], NULL);</FONT>
   }
   printf ("Main(): Waited on %d  threads. Done.\n", NUM_THREADS);

   <FONT COLOR=#AAAAAA>/* Clean up and exit */</FONT>
   <FONT Color=#DF4442>pthread_attr_destroy(&attr);</FONT>
   <FONT Color=#DF4442>pthread_mutex_destroy(&count_mutex);</FONT>
   <FONT Color=#DF4442>pthread_cond_destroy(&count_threshold_cv);</FONT>
   <FONT Color=#DF4442>pthread_exit(NULL);</FONT>

 } 

</PRE>
