C******************************************************************************
C FILE: omp_bug4.f
C DESCRIPTION:
C   This very simple program causes a segmentation fault.
C AUTHOR: Blaise Barney  01/09/04
C LAST REVISED: 
C******************************************************************************

      PROGRAM BUG4
     
      INTEGER N, NTHREADS, TID, I, J, OMP_GET_NUM_THREADS,
     +        OMP_GET_THREAD_NUM
      PARAMETER(N=1048)
      REAL*8 A(N,N)
  
C     Fork a team of threads with explicit variable scoping
!$OMP PARALLEL SHARED(NTHREADS) PRIVATE(I,J,TID,A)

C     Obtain/print thread info
      TID = OMP_GET_THREAD_NUM()
      IF (TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Number of threads = ', NTHREADS
      END IF
      PRINT *, 'Thread',TID,' starting...'

C     Each thread works on its own private copy of the array
      DO I=1,N
        DO J=1,N
          A(J,I) = TID + I + J
        END DO
      END DO

C     For confirmation
      PRINT *, 'Thread',TID,'done. Last element=',A(N,N)

C     All threads join master thread and disband
!$OMP END PARALLEL

      END

