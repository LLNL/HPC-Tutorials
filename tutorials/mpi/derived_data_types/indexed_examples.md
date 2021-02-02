#### C Language - Indexed Derived Data Type Example

```	
#include "mpi.h"
#include <stdio.h>
#define NELEMENTS 6

main(int argc, char *argv[])  {
int numtasks, rank, source=0, dest, tag=1, i;
int blocklengths[2], displacements[2];
float a[16] = 
    {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 
    9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0};
float b[NELEMENTS]; 

MPI_Status stat;
MPI_Datatype indextype;   // required variable

MPI_Init(&argc,&argv);
MPI_Comm_rank(MPI_COMM_WORLD, &rank);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

blocklengths[0] = 4;
blocklengths[1] = 2;
displacements[0] = 5;
displacements[1] = 12;

// create indexed derived data type
MPI_Type_indexed(2, blocklengths, displacements, MPI_FLOAT, &indextype);
MPI_Type_commit(&indextype);

if (rank == 0) {
    for (i=0; i<numtasks; i++) 
    // task 0 sends one element of indextype to all tasks
    MPI_Send(a, 1, indextype, i, tag, MPI_COMM_WORLD);
    }

// all tasks receive indextype data from task 0
MPI_Recv(b, NELEMENTS, MPI_FLOAT, source, tag, MPI_COMM_WORLD, &stat);
printf("rank= %d  b= %3.1f %3.1f %3.1f %3.1f %3.1f %3.1f\n",
        rank,b[0],b[1],b[2],b[3],b[4],b[5]);

// free datatype when done using it
MPI_Type_free(&indextype);
MPI_Finalize();
}
```

#### Fortran - Indexed Derived Data Type Example

```
program indexed
include 'mpif.h'

integer NELEMENTS
parameter(NELEMENTS=6)
integer numtasks, rank, source, dest, tag, i,  ierr
integer blocklengths(0:1), displacements(0:1)
real*4 a(0:15), b(0:NELEMENTS-1)
integer stat(MPI_STATUS_SIZE)
integer indextype   ! required variable
tag = 1

data a  /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, &
        9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0 /

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

blocklengths(0) = 4
blocklengths(1) = 2
displacements(0) = 5
displacements(1) = 12

! create indexed derived data type
call MPI_TYPE_INDEXED(2, blocklengths, displacements, MPI_REAL, &
                        indextype, ierr)
call MPI_TYPE_COMMIT(indextype, ierr)

if (rank .eq. 0) then
    ! task 0 sends one element of indextype to all tasks
    do i=0, numtasks-1
    call MPI_SEND(a, 1, indextype, i, tag, MPI_COMM_WORLD, ierr)
    end do
endif

! all tasks receive indextype data from task 0
source = 0
call MPI_RECV(b, NELEMENTS, MPI_REAL, source, tag, MPI_COMM_WORLD, &
                stat, ierr)
print *, 'rank= ',rank,' b= ',b

! free datatype when done using it
call MPI_TYPE_FREE(indextype, ierr)
call MPI_FINALIZE(ierr)

end
```

#### Output

Sample program output:

```
rank= 0  b= 6.0 7.0 8.0 9.0 13.0 14.0
rank= 1  b= 6.0 7.0 8.0 9.0 13.0 14.0
rank= 2  b= 6.0 7.0 8.0 9.0 13.0 14.0
rank= 3  b= 6.0 7.0 8.0 9.0 13.0 14.0
```