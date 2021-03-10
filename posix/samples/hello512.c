/******************************************************************************
* FILE: hello512.c
* DESCRIPTION:
*   A "hello world" Pthreads program which creates a large number of 
*   threads per AIX process.  A sleep() call is used to insure that all
*   threads are in existence at the same time.  Each Hello thread does some
*   work to demonstrate how the AIX scheduler behavior affects thread 
*   completion order.
*
* SOURCE: 9/98 Blaise Barney
* LAST REVISED: 9/20/98 Blaise Barney
******************************************************************************/

#include <pthread.h>
#include <stdio.h>
#define NTHREADS	512

void *Hello(void *threadid)
{
   int i;
   double result=0.0;
   sleep(3);
   for (i=0; i<1000; i++)
   {
     result = result + (double)random(); 
   }
   printf("%d: Hello World!\n", threadid);
   pthread_exit(NULL);
}

int main(int argc, char *argv[])
{
   pthread_t threads[NTHREADS];
   int rc, t;
   for(t=0;t<NTHREADS;t++){
      rc = pthread_create(&threads[t], NULL, Hello, (void *)t);
      if (rc){
         printf("ERROR: return code from pthread_create() is %d\n", rc);
         printf("Code %d= %s\n",rc,strerror(rc));
         exit(-1);
      }
   }
   printf("main(): Created %d threads.\n", t);
   pthread_exit(NULL);
}
