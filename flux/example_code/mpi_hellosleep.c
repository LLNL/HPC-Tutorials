#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#define  MASTER		0

int main (int argc, char *argv[])
{
int   numtasks, taskid, len;
char hostname[MPI_MAX_PROCESSOR_NAME];

MPI_Init(&argc, &argv);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
MPI_Comm_rank(MPI_COMM_WORLD,&taskid);
MPI_Get_processor_name(hostname, &len);
if (taskid == MASTER){
   printf("MASTER: Number of MPI tasks is: %d\n",numtasks);
   fflush(stdout);
}
printf ("task %d on %s going to sleep\n", taskid, hostname);
fflush(stdout);
sleep(300);
printf ("task %d on %s woke up\n", taskid, hostname);
fflush(stdout);
MPI_Finalize();

}
