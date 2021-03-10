/******************************************************************************
* FILE: hellodump.c
* DESCRIPTION:
*   A "hello world" Pthreads program that dumps core.  Figure out why and
*   then fix it.
* AUTHOR: 9/98 Blaise Barney
* LAST REVISED: 04/05/05
******************************************************************************/
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#define NTHREADS	8
#define ARRAY_SIZE      32000

void *Hello(void *threadid)
{
   double A[ARRAY_SIZE];
   int i;
   sleep(3);
   for (i=0; i<ARRAY_SIZE; i++)
   {
      A[i] = i * 1.0;
   }
   printf("%d: Hello World!   %f\n", threadid, A[ARRAY_SIZE-1]);
   pthread_exit(NULL);
}

int main(int argc, char *argv[])
{
   pthread_t threads[NTHREADS];
   size_t stacksize;
   pthread_attr_t attr;
   int rc, t;
   pthread_attr_init(&attr);
   pthread_attr_getstacksize (&attr, &stacksize);
   printf("Thread stack size = %d bytes (hint, hint)\n",stacksize);
   for(t=0;t<NTHREADS;t++){
      rc = pthread_create(&threads[t], NULL, Hello, (void *)t);
      if (rc){
         printf("ERROR; return code from pthread_create() is %d\n", rc);
         exit(-1);
      }
   }
   printf("Created %d threads.\n", t);
   pthread_exit(NULL);
}
