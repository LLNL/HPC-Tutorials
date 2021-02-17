C ****************************************************************************
C FILE: mpi_bug5.f
C   This is an "unsafe" program. It's behavior varies depending upon the
C   platform and MPI library
C AUTHOR: Blaise Barney 
C LAST REVISED: 01/24/09
C ****************************************************************************

      program mpi_bug5
      include 'mpif.h'

      integer   MSGSIZE
      parameter (MSGSIZE = 2000)
      
      integer numtasks, rank, i, tag, dest, source, count, forever
      character data(MSGSIZE)
      real*8 start, end, result
      real anum
      integer status(MPI_STATUS_SIZE)
     
      tag = 111
      dest = 1
      source = 0
      count = 0
      forever = 1

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

      if (rank .eq. 0) then
        print *, 'mpi_bug5 has started...'
        if (numtasks .gt. 2) then
          print *, 'INFO: Number of tasks=',numtasks,
     +             '. Only using 2 tasks.'
        endif
      endif


C******************************* Send task **********************************/
      if (rank .eq. 0) then
C       Initialize send data
        do i=1, MSGSIZE
          data(i) = 'x'
        enddo

        start = MPI_WTIME()
        do while (forever .eq. 1)
          call MPI_SEND(data, MSGSIZE, MPI_BYTE, dest, tag, 
     +                  MPI_COMM_WORLD, ierr)
          count = count + 1
          i = MOD(count, 10)
          if (i .eq. 0) then
            end = MPI_WTIME()
            print *, 'Count=',count,'Time=',end-start,'sec.'
            start = MPI_WTIME()
          endif
        enddo
      endif


C****************************** Receive task ********************************/

      if (rank .eq. 1) then
        do while (forever .eq. 1)
          call MPI_RECV(data, MSGSIZE, MPI_BYTE, source, tag, 
     +         MPI_COMM_WORLD, status, ierr)
C         Do some work  - at least more than the send task
          result = 0.0
          do i=1, 1000000
            call RANDOM_NUMBER(anum)
            result = result + anum
          enddo
        enddo
      endif

      call MPI_FINALIZE(ierr)
      end
