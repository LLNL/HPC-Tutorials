C ****************************************************************************
C FILE: mpi_vector.f
C DESCRIPTION:
C   MPI tutorial example code: Vector Derived Datatype
C AUTHOR: Blaise Barney
C LAST REVISED: 04/02/05
C ****************************************************************************

      program vector
      include 'mpif.h'

      integer SIZE
      parameter(SIZE=4)
      integer numtasks, rank, source, dest, tag, i,  ierr
      real*4 a(0:SIZE-1,0:SIZE-1), b(0:SIZE-1)
      integer stat(MPI_STATUS_SIZE), rowtype

C     Fortran stores this array in column major order
      data a  /1.0, 2.0, 3.0, 4.0, 
     &         5.0, 6.0, 7.0, 8.0,
     &         9.0, 10.0, 11.0, 12.0, 
     &         13.0, 14.0, 15.0, 16.0 /

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

      call MPI_TYPE_VECTOR(SIZE, 1, SIZE, MPI_REAL, rowtype, ierr)
      call MPI_TYPE_COMMIT(rowtype, ierr)
  
      tag = 1
      if (numtasks .eq. SIZE) then
         if (rank .eq. 0) then
            do 10 i=0, numtasks-1
            call MPI_SEND(a(i,0), 1, rowtype, i, tag,
     &                    MPI_COMM_WORLD, ierr)
  10        continue
         endif

         source = 0
         call MPI_RECV(b, SIZE, MPI_REAL, source, tag, 
     &                MPI_COMM_WORLD, stat, ierr)
         print *, 'rank= ',rank,' b= ',b

      else
         print *, 'Must specify',SIZE,' processors.  Terminating.' 
      endif

      call MPI_TYPE_FREE(rowtype, ierr)
      call MPI_FINALIZE(ierr)

      end
