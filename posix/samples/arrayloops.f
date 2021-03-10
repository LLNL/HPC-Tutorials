C-----------------------------------------------------------------------------
C FILE: arrayloops.f
C DESCRIPTION:
C   Example code demonstrating decomposition of array processing by
C   distributing loop iterations.  A global sum is maintained by a mutex
C   variable.  The Fortran example can be compiled with the command (IBM only):
C   xlf_r -O2 -qsmp=omp -qnosave arrayloops.f
C   Note also the need for the f_pthread module.
C AUTHOR: Blaise Barney
C LAST REVISED: 02/11/2002 Blaise Barney
C-----------------------------------------------------------------------------

      program arrayloops

C     This IBM Pthreads library module is required
      use f_pthread

C     Globals
      integer NTHREADS, ARRAYSIZE, ITERATIONS
      parameter(NTHREADS=4)
      parameter(ARRAYSIZE=1000000)
      parameter(ITERATIONS=ARRAYSIZE/NTHREADS)
      real*8 A(ARRAYSIZE), sum
      type(f_pthread_mutex_t) sum_mutex
      common /global/ A, sum, sum_mutex

C     Local variables
      integer rc, i, tids(NTHREADS)
      type(f_pthread_t) threads(NTHREADS)
      type(f_pthread_attr_t) attr

C     This will be the routine which threads execute
      external do_work

C     Pthreads setup:  Initialize mutex and explicitly create threads in a
C     joinable state (for portability).  Pass each thread its loop  offset
      rc = f_pthread_mutex_init(sum_mutex)
      rc = f_pthread_attr_init(attr)
      rc = f_pthread_attr_setdetachstate(attr,
     +                               PTHREAD_CREATE_JOINABLE)
      sum = 0
      do 100 i=1, NTHREADS
      tids(i) = i
      rc = f_pthread_create(threads(i),attr,FLAG_DEFAULT,
     +                      do_work,tids(i))
  100 continue

C     Wait for all threads to complete then print global sum 
      do 200 i=1,NTHREADS
        rc = f_pthread_join(threads(i))
  200 continue
      write(*,*)'Done. Sum= ',sum

C     Clean up and exit
      rc = f_pthread_attr_destroy(attr)
      rc = f_pthread_mutex_destroy(sum_mutex)
      call f_pthread_exit(rc)
      end

C-----------------------------------------------------------------------------

      subroutine do_work(mytid)

C     This IBM Pthreads library module is required
      use f_pthread

C     Globals
      integer NTHREADS, ARRAYSIZE, ITERATIONS
      parameter(NTHREADS=4)
      parameter(ARRAYSIZE=1000000)
      parameter(ITERATIONS=ARRAYSIZE/NTHREADS)
      real*8 A(ARRAYSIZE), sum
      type(f_pthread_mutex_t) sum_mutex
      common /global/ A, sum, sum_mutex

C     Local variables
      integer rc, mytid, start, end, i
      real*8 mysum

C     Initialize my part of the global array and keep local sum 
      start = (mytid-1) * ITERATIONS +1
      end = start + ITERATIONS - 1
      write(*,*) 'Thread ',mytid,' doing iterations ',start,
     +           ' to ',end
      mysum = 0
      do 100 i=start,end
        A(i) = i * 1.0
        mysum = mysum + a(i)
  100 continue

C     Lock the mutex and update the global sum, then exit
      rc = f_pthread_mutex_lock(sum_mutex)
      sum = sum + mysum
      rc = f_pthread_mutex_unlock(sum_mutex)
      call f_pthread_exit(rc)
      end subroutine do_work



