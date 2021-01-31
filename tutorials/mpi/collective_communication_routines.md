---
layout: page
title: "Collective Communication Routines"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

![collective_comm](images/collective_comm.gif)

#### Types of Collective Operations: 

* **Synchronization** - processes wait until all members of the group have reached the synchronization point.
* **Data Movement** - broadcast, scatter/gather, all to all.
* **Collective Computation** (reductions) - one member of the group collects data from the other members and performs an operation (min, max, add, multiply, etc.) on that data.

#### Scope:

Collective communication routines must involve all processes within the scope of a communicator. All processes are by default, members in the communicator MPI_COMM_WORLD. Additional communicators can be defined by the programmer. See the [Group and Communicator Management Routines](management_routines) section for details.

Unexpected behavior, including program failure, can occur if even one task in the communicator doesn't participate.

It is the programmer's responsibility to ensure that all processes within a communicator participate in any collective operations.

#### Programming Considerations and Restrictions:

Collective communication routines do not take message tag arguments.

Collective operations within subsets of processes are accomplished by first partitioning the subsets into new groups and then attaching the new groups to new communicators (discussed in the [Group and Communicator Management Routines](management_routines) section).

Can only be used with MPI predefined datatypes - not with MPI [Derived Data Types](derived_data_types).

MPI-2 extended most collective operations to allow data movement between intercommunicators (not covered here).

With MPI-3, collective operations can be blocking or non-blocking. Only blocking operations are covered in this tutorial.

### Collective Communication Routines

[**MPI_Barrier**](collective_comms/MPI_Barrier.txt)

Synchronization operation. Creates a barrier synchronization in a group. Each task, when reaching the MPI_Barrier call, blocks until all tasks in the group reach the same MPI_Barrier call. Then all tasks are free to proceed.

```
MPI_Barrier (comm)
MPI_BARRIER (comm,ierr)
```

[**MPI_Bcast**](collective_comms/MPI_Bcast.txt)

Data movement operation. Broadcasts (sends) a message from the process with rank "root" to all other processes in the group. 

```
MPI_Bcast (&buffer,count,datatype,root,comm) 
MPI_BCAST (buffer,count,datatype,root,comm,ierr)
```

[**MPI_Scatter**](collective_comms/MPI_Scatter.txt)

[**MPI_Gather**](collective_comms/MPI_Gather.txt)

[**MPI_Allgather**](collective_comms/MPI_Allgather.txt)

[**MPI_Reduce**](collective_comms/MPI_Reduce.txt)

[**MPI_Allreduce**](collective_comms/MPI_Allreduce.txt)

[**MPI_Reduce_scatter**](collective_comms/MPI_Reduce_scatter.txt)

[**MPI_Alltoall**](collective_comms/MPI_Alltoall.txt)

[**MPI_Scan**](collective_comms/MPI_Scan.txt)

### Examples

#### C Language - Collective Communications Example
```
#include "mpi.h"
#include <stdio.h>
#define SIZE 4

main(int argc, char *argv[])  {
int numtasks, rank, sendcount, recvcount, source;
float sendbuf[SIZE][SIZE] = {
    {1.0, 2.0, 3.0, 4.0},
    {5.0, 6.0, 7.0, 8.0},
    {9.0, 10.0, 11.0, 12.0},
    {13.0, 14.0, 15.0, 16.0}  };
float recvbuf[SIZE];

MPI_Init(&argc,&argv);
MPI_Comm_rank(MPI_COMM_WORLD, &rank);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

if (numtasks == SIZE) {
    // define source task and elements to send/receive, then perform collective scatter
    source = 1;
    sendcount = SIZE;
    recvcount = SIZE;
    MPI_Scatter(sendbuf,sendcount,MPI_FLOAT,recvbuf,recvcount,
                MPI_FLOAT,source,MPI_COMM_WORLD);

    printf("rank= %d  Results: %f %f %f %f\n",rank,recvbuf[0],
        recvbuf[1],recvbuf[2],recvbuf[3]);
    }
else
    printf("Must specify %d processors. Terminating.\n",SIZE);

MPI_Finalize();
}
```

#### Fortran - Collective Communications Example

```
program scatter
include 'mpif.h'

integer SIZE
parameter(SIZE=4)
integer numtasks, rank, sendcount, recvcount, source, ierr
real*4 sendbuf(SIZE,SIZE), recvbuf(SIZE)

! Fortran stores this array in column major order, so the 
! scatter will actually scatter columns, not rows.
data sendbuf /1.0, 2.0, 3.0, 4.0, &
                5.0, 6.0, 7.0, 8.0, &
                9.0, 10.0, 11.0, 12.0, &
                13.0, 14.0, 15.0, 16.0 /

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

if (numtasks .eq. SIZE) then
    ! define source task and elements to send/receive, then perform collective scatter
    source = 1
    sendcount = SIZE
    recvcount = SIZE
    call MPI_SCATTER(sendbuf, sendcount, MPI_REAL, recvbuf, recvcount, MPI_REAL, &
                    source, MPI_COMM_WORLD, ierr)

    print *, 'rank= ',rank,' Results: ',recvbuf 

else
    print *, 'Must specify',SIZE,' processors.  Terminating.' 
endif

call MPI_FINALIZE(ierr)

end
```

Sample program output:

```
rank= 0  Results: 1.000000 2.000000 3.000000 4.000000
rank= 1  Results: 5.000000 6.000000 7.000000 8.000000
rank= 2  Results: 9.000000 10.000000 11.000000 12.000000
rank= 3  Results: 13.000000 14.000000 15.000000 16.000000
```
