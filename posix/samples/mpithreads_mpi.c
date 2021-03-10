/*****************************************************************************
* FILE: mpithreads_mpi.c
* DESCRIPTION:
*   This simple program illustrates the use of MPI in a program obtained 
*   by modifying a serial code that performs a dot product. It is the third 
*   of four codes used to show the progression from a serial program to a 
*   hybrid MPI/Pthreads program.  The other relevant codes are:
*      - mpithreads_serial.c   - The serial version
*      - mpithreads_threads.c  - A shared memory programming model using
*          Pthreads
*      - mpithreads_both.c - A hybrid model that utilizes both MPI and
*          Pthreads to execute on systems that are comprised of clusters
*          of SMP's.
*   Use of the SPMD model was chosen and for convenience, with replication 
*   of the main data on all nodes. A more memory efficient implementation 
*   would be advisable for larger data sets.
* SOURCE: Vijay Sonnad, IBM
* LAST REVISED:  01/29/09 Blaise Barney
******************************************************************************/
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

/*   
The following structure contains the necessary information to allow the 
function "dotprod" to access its input data and place its output into 
the structure.  Note that this structure is unchanged from the sequential 
version.
*/

typedef struct 
 {
   double      *a;
   double      *b;
   double     sum; 
   int     veclen; 
 } DOTDATA;

/* Define globally accessible variables */

#define VECLEN 100
DOTDATA dotstr; 

/*
The function dotprod is very similar to the sequential version except that 
we now have each node working on a different part of the data. As before, 
all access to the input is through a structure of type DOTDATA and all 
output from this function is written into this same structure. 
*/

void *dotprod()
{

   /* Define and use local variables for convenience */

   int i, start, end, myid, len;
   double mysum, *x, *y;
   
   /* Obtain rank of this node */

   MPI_Comm_rank (MPI_COMM_WORLD, &myid);
     
   len = dotstr.veclen;
   start = myid*len;
   end   = start + len;
   x = dotstr.a;
   y = dotstr.b;

   /*
   Perform the dot product and assign result to the appropriate variable in 
   the structure. 
   */

   mysum = 0;
   for (i=start; i<end ; i++) 
    {
      mysum += (x[i] * y[i]);
    }

   dotstr.sum += mysum;

}

/* 
As before,the main program does very little computation. It does however make 
all the calls to the MPI routines. This is not a master-worker arrangement 
and all nodes participate equally in the work. 
*/

int main (int argc, char* argv[])
{
int i,len=VECLEN;
int myid, numprocs;
double *a, *b;
double mysum, allsum;

/* MPI Initialization */
MPI_Init (&argc, &argv);
MPI_Comm_size (MPI_COMM_WORLD, &numprocs);
MPI_Comm_rank (MPI_COMM_WORLD, &myid);

/* Assign storage and initialize values */
a = (double*) malloc (numprocs*len*sizeof(double));
b = (double*) malloc (numprocs*len*sizeof(double));
  
for (i=0; i<len*numprocs; i++) {
  a[i]=1;
  b[i]=a[i];
  }

dotstr.veclen = len; 
dotstr.a = a; 
dotstr.b = b; 
dotstr.sum=0;

/* Call  the  dot product routine */
dotprod();
mysum = dotstr.sum;
printf("Task %d partial sum is %f\n",myid, mysum);

/* After the dot product, perform a summation of results on each node */
MPI_Reduce (&mysum, &allsum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

if (myid == 0) 
  printf ("Done. MPI version: sum  =  %f \n", allsum);
free (a);
free (b);
MPI_Finalize();
}   
