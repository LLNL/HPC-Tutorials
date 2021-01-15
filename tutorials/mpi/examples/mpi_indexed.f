C ****************************************************************************
C FILE: mpi_indexed.f
C DESCRIPTION:
C   MPI tutorial example code: Indexed Derived Datatype
C AUTHOR: Blaise Barney
C LAST REVISED: 04/02/05
C ****************************************************************************

      program indexed
      include 'mpif.h'

      integer NELEMENTS
      parameter(NELEMENTS=6)
      integer numtasks, rank, source, dest, tag, i,  ierr
      integer blocklengths(0:1), displacements(0:1)
      real*4 a(0:15), b(0:NELEMENTS-1)
      integer stat(MPI_STATUS_SIZE), indextype

      data a  /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0,
     &         9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0 /

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

      blocklengths(0) = 4
      blocklengths(1) = 2
      displacements(0) = 5
      displacements(1) = 12

      call MPI_TYPE_INDEXED(2, blocklengths, displacements, MPI_REAL, 
     &                      indextype, ierr)
      call MPI_TYPE_COMMIT(indextype, ierr)
  
      tag = 1
      if (rank .eq. 0) then
         do 10 i=0, numtasks-1
         call MPI_SEND(a, 1, indextype, i, tag, MPI_COMM_WORLD, ierr)
  10     continue
      endif

      source = 0
      call MPI_RECV(b, NELEMENTS, MPI_REAL, source, tag, MPI_COMM_WORLD, 
     &              stat, ierr)
      print *, 'rank= ',rank,' b= ',b

      call MPI_TYPE_FREE(indextype, ierr)
      call MPI_FINALIZE(ierr)

      end
