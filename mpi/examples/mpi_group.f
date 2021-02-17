C ****************************************************************************
C FILE: mpi_group.f
C DESCRIPTION:
C   MPI tutorial example code: Groups/Communicators
C AUTHOR: Blaise Barney
C LAST REVISED: 04/02/05
C ****************************************************************************
      program group
      include 'mpif.h'

      integer NPROCS
      parameter(NPROCS=8)
      integer rank, new_rank, sendbuf, recvbuf, numtasks
      integer ranks1(4), ranks2(4), ierr
      integer orig_group, new_group, new_comm
      data ranks1 /0, 1, 2, 3/, ranks2 /4, 5, 6, 7/

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

      if (numtasks .ne. NPROCS) then
        print *, 'Must specify ',NPROCS,' tasks. Terminating.'
        call MPI_FINALIZE(ierr)
        stop
      endif

      sendbuf = rank

C     Extract the original group handle
      call MPI_COMM_GROUP(MPI_COMM_WORLD, orig_group, ierr)

C     Divide tasks into two distinct groups based upon rank
      if (rank .lt. NPROCS/2) then
         call MPI_GROUP_INCL(orig_group, NPROCS/2, ranks1, 
     &                       new_group, ierr)
      else 
         call MPI_GROUP_INCL(orig_group, NPROCS/2, ranks2, 
     &                       new_group, ierr)
      endif

      call MPI_COMM_CREATE(MPI_COMM_WORLD, new_group, 
     &                       new_comm, ierr)
      call MPI_ALLREDUCE(sendbuf, recvbuf, 1, MPI_INTEGER, MPI_SUM,
     &               new_comm, ierr)

      call MPI_GROUP_RANK(new_group, new_rank, ierr)
      print *, 'rank= ',rank,' newrank= ',new_rank,' recvbuf= ',
     &     recvbuf

      call MPI_FINALIZE(ierr)
      end
