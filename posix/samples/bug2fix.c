/******************************************************************************
* FILE: bug2fix.c
* DESCRIPTION:
*   This is just one way to fix the "hello world" Pthreads program that dumps 
*   core.  Things to note:
*     - attr variable and its scoping
*     - use of the pthread_attr_setstacksize routine
*     - initialization of the attr variable with pthread_attr_init
*     - passing the attr variable to pthread_create
* AUTHOR: 9/98 Blaise Barney 
* LAST REVISED: 01/29/09
******************************************************************************/
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#define NTHREADS	8
#define ARRAY_SIZE      500000
#define MEGEXTRA        1000000

pthread_attr_t attr;

void *Hello(void *threadid)
{
   double A[ARRAY_SIZE];
   int i;
   long tid;
   size_t mystacksize;

   tid = (long)threadid;
   sleep(3);
   for (i=0; i<ARRAY_SIZE; i++)
   {
      A[i] = i * 1.0;
   }
   printf("%ld: Hello World!   %f\n", tid, A[ARRAY_SIZE-1]);
   pthread_attr_getstacksize (&attr, &mystacksize);
   printf("%ld: Thread stack size = %li bytes \n", tid, mystacksize);
   pthread_exit(NULL);
}

int main(int argc, char *argv[])
{
pthread_t threads[NTHREADS];
size_t stacksize;
int rc; 
long t;
pthread_attr_init(&attr);
stacksize = ARRAY_SIZE*sizeof(double) + MEGEXTRA;
pthread_attr_setstacksize (&attr, stacksize);
pthread_attr_getstacksize (&attr, &stacksize);
printf("Thread stack size = %li bytes (hint, hint)\n",stacksize);
for(t=0;t<NTHREADS;t++){
  rc = pthread_create(&threads[t], &attr, Hello, (void *)t);
    if (rc){
      printf("ERROR; return code from pthread_create() is %d\n", rc);
      exit(-1);
      }
  }
printf("Created %ld threads.\n", t);
pthread_exit(NULL);
}
