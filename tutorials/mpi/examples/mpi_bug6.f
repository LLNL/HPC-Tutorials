C******************************************************************************
C FILE: mpi_bug6.f
C DESCRIPTION:
C   This program attempts to compare isend/irecv with send/irecv, but there is
C   a bug. Under AIX it will cause program failure. Intel Linux ignores it. 
C   4 tasks are required.
C SOURCE: Blaise Barney 
C LAST REVISED: 04/02/05
C*****************************************************************************/

      program mpi_bug6
      include 'mpif.h'

      integer REPS, DISP, numtasks, rank, buf, tag, i, tmp,
     +        ierr, dest, src, offset, nreqs
      parameter (DISP=100)
      parameter (REPS=1000)
      integer reqs(REPS*2), stats(MPI_STATUS_SIZE,REPS*2) 
      real*8 T1, T2

      tag = 1

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)

C     Require 4 tasks
      if (rank .EQ. 0) then
        if (numtasks .NE. 4) then
          print *, 'ERROR: Number of tasks must be 4. Quitting.'
          call MPI_ABORT(MPI_COMM_WORLD, ierr)
        endif
        print *, 'Starting isend/irecv send/irecv test...'
      endif

C     Use barriers for clean output
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
      print *, 'Task',rank,'starting...'
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)

C     Start the clock
      T1 = MPI_WTIME()
      
C     Tasks 0 and 1 do the isend/irecv test.
C     Determine who to send/receive with. nreqs specifies how many non-blocking
C     operation request handles to capture. offset is where the task should
C     store each request as it is captured in the reqs() array. 
      if (rank .LT. 2) then
        nreqs = REPS*2
        if (rank .EQ. 0) then
          src = 1
          offset = 1
        else
          src = 0
          offset = REPS
        endif
        dest = src

C       Do the non-blocking send and receive operations
        do i=1,REPS 
          call MPI_ISEND(rank, 1, MPI_INTEGER, dest, tag, 
     +                   MPI_COMM_WORLD, reqs(offset), ierr)
          call MPI_IRECV(buf, 1, MPI_INTEGER, src, tag, 
     +                   MPI_COMM_WORLD, reqs(offset+1), ierr)
          offset = offset + 2
          tmp = MOD(i, DISP)
          if (tmp .EQ. 0) then
            print *, 'Task',rank,'has done',i,'isends/irecvs'
          endif
        enddo
      endif

C     Tasks 2 and 3 do the send/irecv test. 
C     Determine who to send/receive with. nreqs specifies how many non-blocking
C     operation request handles to capture. offset is where the task should
C     store each request as it is captured in the reqs() array. 
      if (rank .GT. 1) then
        nreqs = REPS

C       Task 2 does the blocking send operation
        if (rank .EQ. 2) then
          dest = 3
          do i=1,REPS
            call MPI_SEND(rank, 1, MPI_INTEGER, dest, tag,
     +                    MPI_COMM_WORLD, ierr)
            tmp = MOD(i, DISP)
            if (tmp .EQ. 0) then
              print *, 'Task',rank,'has done',i,'sends'
            endif
          enddo
        endif

C       Task 3 does the non-blocking receive operation
        if (rank .EQ. 3) then
          src = 2
          offset = 1
          do i=1,REPS
            call MPI_IRECV(buf, 1, MPI_INTEGER, src, tag,
     +                   MPI_COMM_WORLD, reqs(offset), ierr)
            offset = offset + 1
            tmp = MOD(i, DISP)
            if (tmp .EQ. 0) then
              print *, 'Task',rank,'has done',i,'irecvs'
            endif
          enddo
        endif

      endif

C     Wait for all non-blocking operations to complete and record time
      call MPI_WAITALL(nreqs, reqs, stats, ierr)
      T2 = MPI_WTIME()
      call MPI_BARRIER(MPI_COMM_WORLD, ierr)

      print *, 'Task',rank,'time(wall)=',T2-T1,' sec'

      call MPI_FINALIZE(ierr)

      end
