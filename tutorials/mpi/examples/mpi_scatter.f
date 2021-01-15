C ****************************************************************************
C FILE: mpi_scatter.f
C DESCRIPTION:
C   MPI tutorial example code: Collective Communications
C AUTHOR: Blaise Barney
C LAST REVISED: 04/02/05
C ****************************************************************************

      program scatter
      include 'mpif.h'

      integer SIZE
      parameter(SIZE=4)
      integer numtasks, rank, sendcount, recvcount, source, ierr
      real*4 sendbuf(SIZE,SIZE), recvbuf(SIZE)

C     Fortran stores this array in column major order, so the 
C     scatter will actually scatter columns, not rows.
      data sendbuf /1.0, 2.0, 3.0, 4.0, 
     &              5.0, 6.0, 7.0, 8.0,
     &              9.0, 10.0, 11.0, 12.0, 
     &              13.0, 14.0, 15.0, 16.0 /

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

      if (numtasks .eq. SIZE) then
         source = 1
         sendcount = SIZE
         recvcount = SIZE
         call MPI_SCATTER(sendbuf, sendcount, MPI_REAL, recvbuf, 
     &        recvcount, MPI_REAL, source, MPI_COMM_WORLD, ierr)
         print *, 'rank= ',rank,' Results: ',recvbuf 
      else
         print *, 'Must specify',SIZE,' processors.  Terminating.' 
      endif

      call MPI_FINALIZE(ierr)

      end
