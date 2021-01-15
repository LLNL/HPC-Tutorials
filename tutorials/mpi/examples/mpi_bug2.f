C ****************************************************************************
C FILE: mpi_bug2.f
C DESCRIPTION: 
C   This program has a bug that causes wrong answers and/or termination - depends
C   upon the MPI library and platform.
C SOURCE: Blaise Barney
C LAST REVISED: 01/24/09
C ****************************************************************************

      program mpi_bug2
      include 'mpif.h'

      integer numtasks, rank, tag, alpha, ierr, i
      real*8 beta
      integer stats(MPI_STATUS_SIZE,10), reqs(10)
      tag = 1

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

      if (rank .eq. 0) then
        if (numtasks .gt. 2) then
          print *, 'Numtasks =',numtasks,' Only 2 needed.
     +    Ignoring extra...'
        endif
        do i=1,10
          alpha= i*10
          call MPI_ISEND(alpha, 1, MPI_INTEGER, 1, tag,
     +         MPI_COMM_WORLD, reqs(i), ierr)
          call MPI_WAIT(reqs(i), stats(1,i), ierr)
          print *, 'Task',rank,'sent =',alpha
        enddo
      endif

      if (rank .eq. 1) then
        do i=1,10
          call MPI_IRECV(beta, 1, MPI_REAL, 0, tag, 
     +         MPI_COMM_WORLD, reqs(i), ierr)
          call MPI_WAIT(reqs(i), stats(1,i), ierr);
          print *, 'Task',rank,'received =',beta
        enddo
      endif

      call MPI_FINALIZE(ierr)

      end
