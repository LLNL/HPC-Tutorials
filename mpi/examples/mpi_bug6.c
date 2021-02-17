/******************************************************************************
* FILE: mpi_bug6.c
* DESCRIPTION:  
*   This program attempts to compare isend/irecv with send/irecv, but there is
*   a bug. Under AIX it will cause program failure. Intel Linux ignores it.
*   4 tasks are required.
* AUTHOER: Blaise Barney 
* LAST REVISED: 04/13/05
******************************************************************************/
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

#define COMM MPI_COMM_WORLD
#define REPS 1000
#define DISP 100

int main (int argc, char *argv[])
{
int numtasks, rank, buf, tag1=1, i, rc, dest, src, offset, nreqs;
double T1, T2;
MPI_Request reqs[REPS*2];
MPI_Status stats[REPS*2];

MPI_Init(&argc,&argv);
MPI_Comm_size(COMM, &numtasks);
MPI_Comm_rank(COMM, &rank);

/* Require 4 tasks */
if (rank == 0 ) {
  if (numtasks != 4) {
    printf("ERROR: Number of tasks must be 4. Quitting.\n");
    MPI_Abort(COMM, rc);
    }
  printf("Starting isend/irecv send/irecv test...\n");
  }

/* Use barriers for clean output */
MPI_Barrier(COMM);
printf("Task %d starting...\n", rank);
MPI_Barrier(COMM);

T1 = MPI_Wtime();     /* start the clock */

/* Tasks 0 and 1 do the isend/irecv test. 
*  Determine who to send/receive with. nreqs specifies how many non-blocking
*  operation request handles to capture. offset is where the task should
*  store each request as it is captured in the reqs() array.         */
if (rank < 2) {
  nreqs = REPS*2;
  if (rank == 0) {
    src = 1;
    offset = 0;
    }
  if (rank == 1) {
    src = 0;
    offset = REPS;
    }
  dest = src;

/* Do the non-blocking send and receive operations */
  for (i=0; i<REPS; i++) {
    MPI_Isend(&rank, 1, MPI_INT, dest, tag1, COMM, &reqs[offset]);
    MPI_Irecv(&buf, 1, MPI_INT, src, tag1, COMM, &reqs[offset+1]);
    offset += 2;
    if ((i+1)%DISP == 0)
      printf("Task %d has done %d isends/irecvs\n", rank, i+1);
    }
  }

/* Tasks 2 and 3 do the send/irecv test. 
   Determine who to send/receive with. nreqs specifies how many non-blocking
   operation request handles to capture. offset is where the task should
   store each request as it is captured in the reqs() array.  */
if (rank > 1) {
  nreqs = REPS;

/* Task 2 does the blocking send operation */
  if (rank == 2) {
    dest = 3;
    for (i=0; i<REPS; i++) {
      MPI_Send(&rank, 1, MPI_INT, dest, tag1, COMM);
      if ((i+1)%DISP == 0)
        printf("Task %d has done %d sends\n", rank, i+1);
      }
    }

/* Task 3 does the non-blocking receive operation */
  if (rank == 3) {
    src = 2;
    offset = 0;
    for (i=0; i<REPS; i++) {
      MPI_Irecv(&buf, 1, MPI_INT, src, tag1, COMM, &reqs[offset]);
      offset += 1;
      if ((i+1)%DISP == 0)
        printf("Task %d has done %d irecvs\n", rank, i+1);
      }
    }

  }

/* Wait for all non-blocking operations to complete and record time */
MPI_Waitall(nreqs, reqs, stats);
T2 = MPI_Wtime();     /* end time */
MPI_Barrier(COMM);

printf("Task %d time(wall)= %lf sec\n", rank, T2-T1);

MPI_Finalize();
}
