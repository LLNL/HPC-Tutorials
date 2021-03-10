/******************************************************************************
* FILE: minim2.c
* DESCRIPTION:
*   This is a very minimal program using Pthreads. The main function 
*   creates 4 threads. Each thread executes a function which prints out the 
*   thread "number".  Creation of a joinable thread is demonstrated by using 
*   the appropriate attributes when creating the thread.
* SOURCE: Vijay Sonnad, IBM
* LAST REVISED: 04/06/05 Blaise Barney
******************************************************************************/
#include <pthread.h>      
#include <stdio.h> 
#include <stdlib.h> 

#define NUMTHDS 4
void *thrdfun (void *arg);

int main(int argc, char *argv[])
{ 
   /*
   We define an array of thread identifiers which are later used in the
   call to create threads.
   */
   pthread_t thrdid[NUMTHDS];
   pthread_attr_t attr;
   int status,i;

   /* Create attribute of JOINABLE for use in creating threads */
   pthread_attr_init(&attr);
   pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

   /* Create  threads using the attribute */
   for (i=0; i<NUMTHDS; i++)
        {
                pthread_create( &thrdid[i], &attr, thrdfun, (void *)i);
        }

   pthread_attr_destroy(&attr);

  
   /* Join with threads after creation  */ 
   for(i=0;i<NUMTHDS;i++)
       {
         if (pthread_join( thrdid[i], (void **)&status)!=0) 
            printf (" Error in join \n");
       }

   pthread_exit(NULL); 
}

/* This function is executed when the thread is created.  */
void *thrdfun (void *arg)
{
   printf ("My thread number = %d \n", (int)arg);
   pthread_exit((void *) 0);
}
