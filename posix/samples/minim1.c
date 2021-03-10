/******************************************************************************
* FILE: minim1.c
* DESCRIPTION:
*   This is a very minimal program using Pthreads. The main function 
*   creates 4 threads. Each thread executes a function which prints out the 
*   thread "number".
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
   We define an array of thread identifiers which are later used as 
   in the call to to create threads.
   */
   pthread_t thrdid[NUMTHDS];
   int i;

   /*   
   Create threads using default values for the attributes,
   (specified by setting NULL as the argument in the call).
   Note that in this version of AIX (which uses Draft7 of
   POSIX.1c) the default is "detached"; thus any attempt to  
   join with any of these threads will result in an error. 
   */

   for (i=0; i<NUMTHDS; i++)
        {
                pthread_create( &thrdid[i], NULL, thrdfun, (void *)i);
        }
   /*
   It is necessary to use pthread_exit at the end of the 
   main program - otherwise, when it exits, all running 
   threads will be killed 
   */
   pthread_exit(NULL); 
}

   /*
   This function is executed when the thread is created.
   Because it was created in a detached state, it will 
   be recycled upon termination, i.e. all data associated 
   with the thread is released. 
   */
void *thrdfun (void *arg)
{
   printf ("My thread number = %d \n", (int)arg);
   pthread_exit((void *) arg);
}
