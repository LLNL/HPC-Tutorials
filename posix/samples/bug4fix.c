/******************************************************************************
* FILE: bug4fix.c
* DESCRIPTION:
*   This is just one way to resolve the synchronization problem demonstrated
*   by bug4.c. A check is made in sub1 to make sure the pthread_cond_wait() 
*   call is not made if the value of count is not what it expects. Its work is
*   also placed after it is awakened, while count is locked.
* SOURCE: 07/06/05 Blaise Barney
* LAST REVISED: 01/29/09  Blaise Barney
******************************************************************************/
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Define and scope what needs to be seen by everyone */
#define NUM_THREADS  3
#define ITERATIONS 10
#define THRESHOLD 12
int count = 0;
double finalresult=0.0;
pthread_mutex_t count_mutex;
pthread_cond_t count_condvar;


void *sub1(void *t)
{
  int i; 
  long tid = (long)t;
  double myresult=0.0;
 
  /*
  Lock mutex and wait for signal only if count is what is expected.  Note
  that the pthread_cond_wait routine will automatically and atomically
  unlock mutex while it waits. Also, note that if THRESHOLD is reached
  before this routine is run by the waiting thread, the loop will be skipped
  to prevent pthread_cond_wait from never returning, and that this thread's
  work is now done within the mutex lock of count.
  */
  pthread_mutex_lock(&count_mutex);
  if (count < THRESHOLD) {
    printf("sub1: thread=%ld going into wait. count=%d\n",tid,count);
    pthread_cond_wait(&count_condvar, &count_mutex);
    printf("sub1: thread=%ld Condition variable signal received.",tid);
    printf(" count=%d\n",count);
    /* do some work */
    sleep(1);
    count++;
    finalresult += myresult;
    printf("sub1: thread=%ld count now equals=%d myresult=%e. Done.\n",
           tid,count,myresult);
    }
  else {
    printf("sub1: count=%d. Not as expected.",count); 
    printf(" Probably missed signal. Skipping work and exiting.\n");
    }
  pthread_mutex_unlock(&count_mutex);
  pthread_exit(NULL);
}

void *sub2(void *t) 
{
  int j,i;
  long tid = (long)t;
  double myresult=0.0;

  for (i=0; i<ITERATIONS; i++) {
    for (j=0; j<100000; j++)
      myresult += sin(j) * tan(i);
    pthread_mutex_lock(&count_mutex);
    finalresult += myresult;
    count++;
    /* 
    Check the value of count and signal waiting thread when condition is
    reached.  Note that this occurs while mutex is locked. 
    */
    if (count == THRESHOLD) {
      printf("sub2: thread=%ld Threshold reached. count=%d. ",tid,count);
      pthread_cond_signal(&count_condvar);
      printf("Just sent signal.\n");
      }
    else {
      printf("sub2: thread=%ld did work. count=%d\n",tid,count);
      }
    pthread_mutex_unlock(&count_mutex);
    }
    printf("sub2: thread=%ld  myresult=%e. Done. \n",tid,myresult);
  pthread_exit(NULL);
}



int main(int argc, char *argv[])
{
  int i, rc; 
  long t1=1, t2=2, t3=3;
  pthread_t threads[3];
  pthread_attr_t attr;

  /* Initialize mutex and condition variable objects */
  pthread_mutex_init(&count_mutex, NULL);
  pthread_cond_init (&count_condvar, NULL);

  /* For portability, explicitly create threads in a joinable state */
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
  pthread_create(&threads[0], &attr, sub1, (void *)t1);
  pthread_create(&threads[1], &attr, sub2, (void *)t2);
  pthread_create(&threads[2], &attr, sub2, (void *)t3);

  /* Wait for all threads to complete */
  for (i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }
  printf ("Main(): Waited on %d threads. Final result=%e. Done.\n",
          NUM_THREADS,finalresult);

  /* Clean up and exit */
  pthread_attr_destroy(&attr);
  pthread_mutex_destroy(&count_mutex);
  pthread_cond_destroy(&count_condvar);
  pthread_exit (NULL);

}

