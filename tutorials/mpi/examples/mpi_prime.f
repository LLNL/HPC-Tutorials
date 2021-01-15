C ****************************************************************************
C FILE: mpi_prime.f
C DESCRIPTION:
C   Generates prime numbers.  All tasks distribute the work evenly, taking
C   every nth number, where n is the stride computed as:  (rank *2) + 1
C   so that even numbers are automatically skipped.  The method of using
C   stride is preferred over contiguous blocks of numbers, since numbers
C   in the higher range require more work to compute and may result in
C   load imbalance.  This program demonstrates embarrassing parallelism.
C   Collective communications calls are used to reduce the only two data
C   elements requiring communications: the number of primes found and
C   the largest prime.
C AUTHOR: Blaise Barney 11/25/95 - adapted from version contributed by 
C   Richard Ng & Wong Sze Cheong during MHPCC Singapore Workshop (8/22/95).
C LAST REVISED: 04/02/05
C ****************************************************************************
C Explanation of constants and variables
C   LIMIT                  = Increase this to find more primes 
C   FIRST                  = Rank of first task 
C   ntasks                 = total number of tasks in partition 
C   rank                   = task identifier 
C   n                      = loop variable 
C   pc                     = prime counter
C   pcsum                  = number of primes found by all tasks 
C   foundone               = most recent prime found 
C   maxprime               = largest prime found 
C   mystart                = where to start calculating 
C   stride                 = calculate every nth number

      program prime
      include 'mpif.h'

      integer LIMIT, FIRST
      parameter(LIMIT=2500000)        
      parameter(FIRST=0)

      integer ntasks, rank, ierr, n, pc, pcsum, foundone, maxprime,
     &        mystart, stride
      double precision start_time, end_time
      logical result

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierr)
      if ((mod(ntasks,2).ne.0) .or. (mod(LIMIT,ntasks).ne.0)) then
         print *, mod(ntasks,2), mod(LIMIT,ntasks)
         print *,'Sorry -this exercise requires an even number of '
         print *,'processors evenly divisible into ',LIMIT,'.' 
         print *,'Try 4 or 8.'
         call MPI_FINALIZE(ierr)
         stop
      endif

C     Initializations: mystart must be odd number. stride is multiplied
C     by 2 to skip over even numbers.  
      start_time = MPI_WTIME()
      mystart = (rank*2) + 1
      stride = ntasks*2
      pc = 0
      foundone = 0

C ------------------- task with rank 0 does this part ------------------
      if (rank .eq. FIRST) then
         print *,'Using',ntasks,'tasks to scan',LIMIT,'numbers'
C        Assume first four primes are counted here
         pc = 4
         do n=mystart,LIMIT,stride
            call isprime(n,result)
            if (result .eqv. .true.) then
               pc = pc + 1
               foundone = n
C              ***** Optional: print each prime as it is found
C              print *, foundone
C              *****
            endif
         enddo
      call MPI_Reduce(pc,pcsum,1,MPI_INTEGER,MPI_SUM,FIRST,
     &                  MPI_COMM_WORLD,ierr)
      call MPI_Reduce(foundone,maxprime,1,MPI_INTEGER,MPI_MAX,
     &                 FIRST,MPI_COMM_WORLD,ierr)
      end_time=MPI_WTIME()
      print *,'Done. Largest prime is ',maxprime,' Total primes ',pcsum
      print *,'Wallclock time elapsed: ' ,end_time-start_time
      endif

C ------------------- all other tasks do this part ---------------------
      if (rank .gt. FIRST) then
         do n=mystart,LIMIT,stride
            call isprime(n,result)
            if (result .eqv. .true.) then
               pc = pc + 1
               foundone = n
C              ***** Optional: print each prime as it is found
C              print *, foundone
C              *****
            endif
         enddo
      call MPI_Reduce(pc,pcsum,1,MPI_INTEGER,MPI_SUM,FIRST,
     &                  MPI_COMM_WORLD,ierr)
      call MPI_Reduce(foundone,maxprime,1,MPI_INTEGER,MPI_MAX,
     &                 FIRST,MPI_COMM_WORLD,ierr)
      endif


      call MPI_FINALIZE(ierr)
      end


      subroutine isprime (n,result)
      integer n
      logical result
      integer i, squareroot
      real*4 realn
      if (n .gt. 10) then
         realn = real(n)
         squareroot = int (sqrt(realn))
         do i=3,squareroot,2
           if (mod(n,i) .eq. 0) then
              result = .false.
              return
           endif
         enddo
         result = .true.
         return 
C Assume first four primes are counted elsewhere. Forget everything else
      else 
        result = .false.
        return
      endif
      end
