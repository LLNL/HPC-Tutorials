C******************************************************************************
C FILE: omp_bug5.f
C DESCRIPTION:
C   Using SECTIONS, two threads initialize their own array and then add 
C   it to the other's array, however a deadlock occurs.
C AUTHOR: Blaise Barney  01/09/04
C LAST REVISED:
C******************************************************************************

      PROGRAM BUG5
     
      INTEGER*8 LOCKA, LOCKB
      INTEGER NTHREADS, TID, I, 
     +        OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
      PARAMETER (N=1000000)
      REAL A(N), B(N), PI, DELTA
      PARAMETER (PI=3.1415926535)
      PARAMETER (DELTA=.01415926535)

C     Initialize the locks
      CALL OMP_INIT_LOCK(LOCKA)
      CALL OMP_INIT_LOCK(LOCKB)

C     Fork a team of threads giving them their own copies of variables
!$OMP PARALLEL SHARED(A, B, NTHREADS, LOCKA, LOCKB) PRIVATE(TID)

C     Obtain thread number and number of threads
      TID = OMP_GET_THREAD_NUM()
!$OMP MASTER
      NTHREADS = OMP_GET_NUM_THREADS()
      PRINT *, 'Number of threads = ', NTHREADS
!$OMP END MASTER
      PRINT *, 'Thread', TID, 'starting...'
!$OMP BARRIER

!$OMP SECTIONS

!$OMP SECTION
      PRINT *, 'Thread',TID,' initializing A()'
      CALL OMP_SET_LOCK(LOCKA)
      DO I = 1, N
         A(I) = I * DELTA
      ENDDO
      CALL OMP_SET_LOCK(LOCKB)
      PRINT *, 'Thread',TID,' adding A() to B()'
      DO I = 1, N
         B(I) = B(I) + A(I)
      ENDDO
      CALL OMP_UNSET_LOCK(LOCKB)
      CALL OMP_UNSET_LOCK(LOCKA)

!$OMP SECTION
      PRINT *, 'Thread',TID,' initializing B()'
      CALL OMP_SET_LOCK(LOCKB)
      DO I = 1, N
         B(I) = I * PI
      ENDDO
      CALL OMP_SET_LOCK(LOCKA)
      PRINT *, 'Thread',TID,' adding B() to A()'
      DO I = 1, N
         A(I) = A(I) + B(I)
      ENDDO
      CALL OMP_UNSET_LOCK(LOCKA)
      CALL OMP_UNSET_LOCK(LOCKB)

!$OMP END SECTIONS NOWAIT

      PRINT *, 'Thread',TID,' done.'

!$OMP END PARALLEL

      END
