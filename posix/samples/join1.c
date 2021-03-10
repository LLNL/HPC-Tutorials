/*****************************************************************************
* FILE: join1.c
* DESCRIPTION:
*   This example demonstrates how to "wait" for thread completions by using
*   the Pthread join routine.  Since not all current implementations of
*   Pthreads create threads in a joinable state, the threads in this
*   example are explicitly created in a joinable state so that they can
*   be joined later.
* AUTHOR: 8/98 Blaise Barney
* LAST REVISED:  04/06/05
******************************************************************************/
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#define NUM_THREADS	3

void *BusyWork(void *null)
{
   int i;
   double result=0.0;
   for (i=0; i<1000000; i++)
   {
     result = result + (double)random();
   }
   printf("Thread result = %e\n",result);
   pthread_exit((void *) 0);
}

int main(int argc, char *argv[])
{
   pthread_t thread[NUM_THREADS];
   pthread_attr_t attr;
   int rc, t;
   void *status;

   /* Initialize and set thread detached attribute */
   pthread_attr_init(&attr);
   pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

   for(t=0;t<NUM_THREADS;t++)
   {
      printf("Creating thread %d\n", t);
      rc = pthread_create(&thread[t], &attr, BusyWork, NULL); 
      if (rc)
      {
         printf("ERROR; return code from pthread_create() is %d\n", rc);
         exit(-1);
      }
   }

   /* Free attribute and wait for the other threads */
   pthread_attr_destroy(&attr);
   for(t=0;t<NUM_THREADS;t++)
   {
      rc = pthread_join(thread[t], &status);
      if (rc)
      {
         printf("ERROR return code from pthread_join() is %d\n", rc);
         exit(-1);
      }
      printf("Completed join with thread %d status= %ld\n",t,(long)status);
   }

   pthread_exit(NULL);
}

