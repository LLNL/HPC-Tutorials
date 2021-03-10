/*****************************************************************************
* FILE: mpithreads_both.c
* DESCRIPTION:
*   This program illustrates the simultaneous use of MPI and Pthreads. 
*   It is essentially a simple combination of a code that implements a dot 
*   product using threads, and a code that uses MPI for the same purpose. 
*   It is the last of four codes used to show the progression from a serial 
*   program to a hybrid MPI/Pthreads program. The other relevant codes are:
*      - mpithreads_serial.c   - The serial version
*      - mpithreads_threads.c  - A shared memory programming model using
*          Pthreads
*      - mpithreads_mpi.c - A distributed memory programming model with MPI
*   All the internode MPI communication is done by the main thread on each 
*   node - the other threads within that node need not even be aware that 
*   internode communication is being performed. Use of the SPMD model for 
*   MPI was chosen for convenience, with replication of the main data on 
*   all nodes. A more memory efficient implementation would be advisable 
*   for larger data sets.  This is the simplest model for mixed MPI/Pthreads 
*   programming. 
* SOURCE: Vijay Sonnad, IBM
* LAST REVISED:  01/29/09 Blaise Barney
******************************************************************************/
#include "mpi.h" 
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/*  
This structure has been changed slightly from the previous cases
to include the number of threads per node. 
*/

typedef struct 
 {
   double      *a;
   double      *b;
   double     sum; 
   int     veclen; 
   int   numthrds;
 } DOTDATA;

/* Define globally accessible variables and a mutex */

#define MAXTHRDS 8
#define VECLEN 100
DOTDATA dotstr; 
pthread_t callThd[MAXTHRDS];
pthread_mutex_t mutexsum;

/*
The function dotprod has only minor changes from the code 
that used threads or MPI.  
*/

void *dotprod(void *arg)
{

   /* Define and use local variables for convenience */

   int i, start, end, len, numthrds, myid;
   long mythrd;
   double mysum, *x, *y;

   /*
   The number of threads and nodes defines the beginning 
   and ending for the dot product; each  thread does work 
   on a vector of length VECLENGTH.
   */

   mythrd = (long)arg;
   MPI_Comm_rank (MPI_COMM_WORLD, &myid);

   numthrds = dotstr.numthrds;
   len = dotstr.veclen;
   start = myid*numthrds*len + mythrd*len;
   end   = start + len;
   x = dotstr.a;
   y = dotstr.b;

   /*
   Perform the dot product and assign result
   to the appropriate variable in the structure. 
   */

   mysum = 0;
   for (i=start; i<end ; i++) 
    {
      mysum += (x[i] * y[i]);
    }

   /*
   Lock a mutex prior to updating the value in the structure, and unlock it 
   upon updating.
   */
   pthread_mutex_lock (&mutexsum);
   printf("Task %d thread %ld adding partial sum of %f to node sum of %f\n",
           myid, mythrd, mysum, dotstr.sum);
   dotstr.sum += mysum;
   pthread_mutex_unlock (&mutexsum);

   pthread_exit((void*)0);
}

/* 
As before,the main program does very little computation. It creates
threads on each node and the main thread does all the MPI calls. 
*/

int main(int argc, char* argv[])
{
int len=VECLEN, myid, numprocs; 
long i;
int nump1, numthrds;
double *a, *b;
double nodesum, allsum;
void *status;
pthread_attr_t attr;

/* MPI Initialization */
MPI_Init (&argc, &argv);
MPI_Comm_size (MPI_COMM_WORLD, &numprocs);
MPI_Comm_rank (MPI_COMM_WORLD, &myid);

/* Assign storage and initialize values */
numthrds=MAXTHRDS;
a = (double*) malloc (numprocs*numthrds*len*sizeof(double));
b = (double*) malloc (numprocs*numthrds*len*sizeof(double));
  
for (i=0; i<len*numprocs*numthrds; i++) {
  a[i]=1;
  b[i]=a[i];
  }

dotstr.veclen = len; 
dotstr.a = a; 
dotstr.b = b; 
dotstr.sum=0;
dotstr.numthrds=MAXTHRDS;
  
/* 
Create thread attribute to specify that the main thread needs
to join with the threads it creates.
*/
pthread_attr_init(&attr );
pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

/* Create a mutex */
pthread_mutex_init (&mutexsum, NULL);

/* Create threads within this node to perform the dotproduct  */
for(i=0;i<numthrds;i++) {
  pthread_create( &callThd[i], &attr, dotprod, (void *)i); 
  }

/* Release the thread attribute handle as it is no longer needed */
pthread_attr_destroy(&attr );

/* Wait on the other threads within this node */
for(i=0;i<numthrds;i++) {
  pthread_join( callThd[i], &status);
  }

nodesum = dotstr.sum;
printf("Task %d node sum is %f\n",myid, nodesum);

/* After the dot product, perform a summation of results on each node */
MPI_Reduce (&nodesum, &allsum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

if (myid == 0)  
printf ("Done. MPI with threads version: sum  =  %f \n", allsum);
MPI_Finalize();
free (a);
free (b);
pthread_mutex_destroy(&mutexsum);
exit (0);
}   
