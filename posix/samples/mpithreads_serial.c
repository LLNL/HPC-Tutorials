/*****************************************************************************
* FILE: mpithreads_serial.c
* DESCRIPTION:
*   This is a simple serial program that computes the dot product of two 
*   vectors.  It is the first of four codes used to show the progression
*   from a serial program to a hybrid MPI/Pthreads program.  The other
*   relevant codes are:
*      - mpithreads_threads.c  - A shared memory programming model using 
*          Pthreads
*      - mpithreads_mpi.c - A distributed memory programming model with MPI
*      - mpithreads_both.c - A hybrid model that utilizes both MPI and 
*          Pthreads to execute on systems that are comprised of clusters 
*          of SMP's.
* SOURCE: Vijay Sonnad, IBM
* LAST REVISED:  01/29/09 Blaise Barney
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>

/*   
The following structure contains the necessary information to allow the 
function "dotprod" to access its input data and place its output so that 
it can be accessed later. 
*/

typedef struct 
 {
   double      *a;
   double      *b;
   double     sum; 
   int    veclen; 
 } DOTDATA;

#define VECLEN 100
DOTDATA dotstr; 

/*
We will use a function (dotprod) to perform the scalar product. All input to 
this routine is obtained through a structure of type DOTDATA and all output 
from this function is written into this same structure.  While this is 
unnecessarily restrictive for a sequential program, it will turn out to be 
useful when we modify the program to compute in parallel.
*/

void* dotprod(void)
{

   /* Define and use local variables for convenience */

   int start, end, i; 
   double mysum, *x, *y;

   start=0;
   end = dotstr.veclen;
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
   dotstr.sum = mysum;

}

/*
The main program initializes data and calls the dotprd() function.  Finally, 
it prints the result.
*/

int main (int argc, char* argv[])
{
int i,len;
double *a, *b;
   
/* Assign storage and initialize values */
len = VECLEN;
a = (double*) malloc (len*sizeof(double));
b = (double*) malloc (len*sizeof(double));
  
for (i=0; i<len; i++) {
  a[i]=1;
  b[i]=a[i];
  }

dotstr.veclen = len; 
dotstr.a = a; 
dotstr.b = b; 
dotstr.sum=0;

/* Perform the  dotproduct */
dotprod ();

/* Print result and release storage */ 
printf ("Done. Serial version: sum =  %f \n", dotstr.sum);
free (a);
free (b);
}
