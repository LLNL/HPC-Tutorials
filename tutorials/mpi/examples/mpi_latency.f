C******************************************************************************
C FILE: mpi_latency.f
C DESCRIPTION:  
C   MPI Latency Timing Program - Fortran Version
C   In this example code, an MPI communication timing test is performed.
C   MPI task 0 will send "reps" number of 1 byte messages to MPI task 1,
C   waiting for a reply between each rep. Before and after timings are made
C   for each rep and an average calculated when completed.
C AUTHOR: Blaise Barney
C LAST REVISED: 03/20/08
C*****************************************************************************
C Variable definitions:
C REPS                    =  number of samples per test 
C tag                     =  MPI message tag parameter 
C numtasks                =  number of MPI tasks 
C rank                    =  my MPI task number 
C dest, source            =  send/receive task designators 
C avgT                    =  average time per rep in microseconds
C ierr                    =  return code 
C T1, T2                  =  start/end times per rep 
C sumT                    =  sum of all reps times 
C deltaT                  =  time for one rep 
C msg                     =  buffer containing 1 byte message 
C status                  =  MPI receive routine parameter

      program latency
      include 'mpif.h'

      integer REPS
      parameter(REPS = 1000)

      integer	rank, numtasks, tag, avgT, ierr, n, 
     &          dest, source, status(MPI_STATUS_SIZE) 
      double precision T1, T2, sumT, deltaT
      character msg

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      if ((rank .eq. 0) .and. (numtasks .ne. 2)) then
        print *, 'Number of tasks = ', numtasks
        print *, 'Only need 2 tasks - extra will be ignored...'
      endif
      tag=1


      if (rank .eq. 0) then
        print *,'task',rank,'has started...'
        print *,'Beginning latency timing test. Number of reps=', REPS 
        print *,'******************************************************'
        print *,'Rep#       T1                T2                 deltaT'
        sumT = 0.0
        dest = 1
        source = 1
        do 10 n = 1, REPS
C         Get start time 
          T1 = MPI_WTIME()
          call MPI_SEND(msg, 1, MPI_CHARACTER, dest, tag, 
     &                  MPI_COMM_WORLD, ierr)
          call MPI_RECV(msg, 1, MPI_CHARACTER, source, tag, 
     &                  MPI_COMM_WORLD, status, ierr)
C	  Get ending time 
          T2 = MPI_WTIME()
          deltaT = T2 - T1
          write(*,9) n, T1, T2, deltaT
 9        format(I4, F22.8,F22.8, F12.8)
          sumT = sumT + deltaT
10      continue   
         
        avgT = (sumT * 1000000) / REPS
C       Print final average from all round trips
        print *,'******************************************************'
        print *,' '
        print *,'*** Avg round trip time=', avgT, 'microseconds' 
        print *,'*** Avg one way latency=', avgT/2, 'microseconds' 
      endif


      if (rank .eq. 1) then
        print *,'task',rank,'has started...'
        dest = 0
        source = 0
        do 20 n = 1, REPS
          call MPI_RECV(msg, 1, MPI_CHARACTER,
     &                  source, tag, MPI_COMM_WORLD, status, ierr)
          call MPI_SEND(msg, 1, MPI_CHARACTER, dest, tag,
     &                  MPI_COMM_WORLD, ierr)
20      continue   
      endif


      call MPI_FINALIZE(ierr)
      end

