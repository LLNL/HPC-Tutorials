
#### C Language - Contiguous Derived Data Type Example

```
#include "mpi.h"
#include <stdio.h>
#define SIZE 4

main(int argc, char *argv[])  {
int numtasks, rank, source=0, dest, tag=1, i;
float a[SIZE][SIZE] =
    {1.0, 2.0, 3.0, 4.0,
    5.0, 6.0, 7.0, 8.0,
    9.0, 10.0, 11.0, 12.0,
    13.0, 14.0, 15.0, 16.0};
float b[SIZE];

MPI_Status stat;
MPI_Datatype rowtype;   // required variable

MPI_Init(&argc,&argv);
MPI_Comm_rank(MPI_COMM_WORLD, &rank);
MPI_Comm_size(MPI_COMM_WORLD, &numtasks);

// create contiguous derived data type
MPI_Type_contiguous(SIZE, MPI_FLOAT, &rowtype);
MPI_Type_commit(&rowtype);

if (numtasks == SIZE) {
    // task 0 sends one element of rowtype to all tasks
    if (rank == 0) {
        for (i=0; i<numtasks; i++)
        MPI_Send(&a[i][0], 1, rowtype, i, tag, MPI_COMM_WORLD);
        }

    // all tasks receive rowtype data from task 0
    MPI_Recv(b, SIZE, MPI_FLOAT, source, tag, MPI_COMM_WORLD, &stat);
    printf("rank= %d  b= %3.1f %3.1f %3.1f %3.1f\n",
            rank,b[0],b[1],b[2],b[3]);
    }
else
    printf("Must specify %d processors. Terminating.\n",SIZE);

// free datatype when done using it
MPI_Type_free(&rowtype);
MPI_Finalize();
}
```

#### Fortran - Contiguous Derived Data Type Example

```
program contiguous
include 'mpif.h'

integer SIZE
parameter(SIZE=4)
integer numtasks, rank, source, dest, tag, i,  ierr
real*4 a(0:SIZE-1,0:SIZE-1), b(0:SIZE-1)
integer stat(MPI_STATUS_SIZE)
integer columntype   ! required variable
tag = 1

! Fortran stores this array in column major order
data a  /1.0, 2.0, 3.0, 4.0, &
        5.0, 6.0, 7.0, 8.0, &
        9.0, 10.0, 11.0, 12.0, & 
        13.0, 14.0, 15.0, 16.0 /

call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

! create contiguous derived data type
call MPI_TYPE_CONTIGUOUS(SIZE, MPI_REAL, columntype, ierr)
call MPI_TYPE_COMMIT(columntype, ierr)

if (numtasks .eq. SIZE) then
    ! task 0 sends one element of columntype to all tasks
    if (rank .eq. 0) then
        do i=0, numtasks-1
        call MPI_SEND(a(0,i), 1, columntype, i, tag, MPI_COMM_WORLD,ierr)
        end do
    endif

    ! all tasks receive columntype data from task 0
    source = 0
    call MPI_RECV(b, SIZE, MPI_REAL, source, tag, MPI_COMM_WORLD, stat, ierr)
    print *, 'rank= ',rank,' b= ',b
else
    print *, 'Must specify',SIZE,' processors.  Terminating.' 
endif

! free datatype when done using it
call MPI_TYPE_FREE(columntype, ierr)
call MPI_FINALIZE(ierr)

end
```

#### Output

Sample program output:

```
rank= 0  b= 1.0 2.0 3.0 4.0
rank= 1  b= 5.0 6.0 7.0 8.0
rank= 2  b= 9.0 10.0 11.0 12.0
rank= 3  b= 13.0 14.0 15.0 16.0
```