/******************************************************************************
* FILE: mpi_helloBsend.c
* DESCRIPTION:
*   MPI tutorial example code: Simple hello world program that uses blocking
*   send/receive routines.
* AUTHOR: Blaise Barney
* LAST REVISED: 06/08/15
******************************************************************************/
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#define  MASTER		0

int main (int argc, char *argv[])
{
int  numtasks, taskid, len, partner, message;
char hostname[MPI_MAX_PROCESSOR_NAME];
MPI_Status status;

MPI_Init(&argc, &argv);
MPI_Comm_rank(MPI_COMM_WORLD, &taskid);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

/* need an even number of tasks  */
if (numtasks % 2 != 0) {
   if (taskid == MASTER) 
      printf("Quitting. Need an even number of tasks: numtasks=%d\n", numtasks);
   } 

else {
   if (taskid == MASTER) 
      printf("MASTER: Number of MPI tasks is: %d\n",numtasks);

   MPI_Get_processor_name(hostname, &len);
   printf ("Hello from task %d on %s!\n", taskid, hostname);

   /* determine partner and then send/receive with partner */
   if (taskid < numtasks/2) {
     partner = numtasks/2 + taskid;
     MPI_Send(&taskid, 1, MPI_INT, partner, 1, MPI_COMM_WORLD);
     MPI_Recv(&message, 1, MPI_INT, partner, 1, MPI_COMM_WORLD, &status);
     }
   else if (taskid >= numtasks/2) {
     partner = taskid - numtasks/2;
     MPI_Recv(&message, 1, MPI_INT, partner, 1, MPI_COMM_WORLD, &status);
     MPI_Send(&taskid, 1, MPI_INT, partner, 1, MPI_COMM_WORLD);
     }

   /* print partner info and exit*/
   printf("Task %d is partner with %d\n",taskid,message);
   }

MPI_Finalize();

}

