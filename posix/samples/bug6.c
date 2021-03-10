/*****************************************************************************
* FILE: bug6.c
* DESCRIPTION:
*   This example demonstrates a race condition with a global variable that
*   gives obviously wrong results. Figure out how to fix the problem - or see
*   bug6fix.c for one solution. The dotprod_mutex.c example provides a much
*   more efficient way of solving this problem than bug6fix.c (FYI).
* SOURCE: 07/06/05 Blaise Barney
* LAST REVISED: 01/29/09 Blaise Barney
******************************************************************************/
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Define global data where everyone can see them */
#define NUMTHRDS 8
#define VECLEN 100000
int *a, *b; 
long sum=0;

void *dotprod(void *arg)
{
  /* Each thread works on a different set of data.
   * The offset is specified by the arg parameter. The size of
   * the data for each thread is indicated by VECLEN.
   */
   int i, start, end, offset, len;
   long tid = (long)arg;
   offset = tid;
   len = VECLEN;
   start = offset*len;
   end   = start + len;

/* Perform my section of the dot product */
   printf("thread: %ld starting. start=%d end=%d\n",tid,start,end-1);
   for (i=start; i<end ; i++) 
      sum += (a[i] * b[i]);
   printf("thread: %ld done. Global sum now is=%li\n",tid,sum);

   pthread_exit((void*) 0);
}



int main (int argc, char *argv[])
{
long i;
void *status;
pthread_t threads[NUMTHRDS];
pthread_attr_t attr;

/* Assign storage and initialize values */
a = (int*) malloc (NUMTHRDS*VECLEN*sizeof(int));
b = (int*) malloc (NUMTHRDS*VECLEN*sizeof(int));
  
for (i=0; i<VECLEN*NUMTHRDS; i++)
  a[i]= b[i]=1;

/* Create threads as joinable, each of which will execute the dot product
 * routine. Their offset into the global vectors is specified by passing
 * the "i" argument in pthread_create().
 */
pthread_attr_init(&attr);
pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
for(i=0; i<NUMTHRDS; i++) 
   pthread_create(&threads[i], &attr, dotprod, (void *)i); 

pthread_attr_destroy(&attr);

/* Wait on the threads for final result */
for(i=0; i<NUMTHRDS; i++) 
  pthread_join(threads[i], &status);

/* After joining, print out the results and cleanup */
printf ("Final Global Sum=%li\n",sum);
free (a);
free (b);
pthread_exit(NULL);
}   

