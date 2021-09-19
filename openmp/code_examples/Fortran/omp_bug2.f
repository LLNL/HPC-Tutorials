C******************************************************************************
C FILE: omp_bug2.f
C DESCRIPTION:
C   Another OpenMP program with a bug
C AUTHOR: Blaise Barney  1/7/04
C LAST REVISED: 
C******************************************************************************

      PROGRAM BUG2
     
      INTEGER NTHREADS, I, TID, OMP_GET_NUM_THREADS,
     +        OMP_GET_THREAD_NUM
      REAL*8 TOTAL
  
C     Spawn parallel region
!$OMP PARALLEL 

C     Obtain thread number
      TID = OMP_GET_THREAD_NUM()
C     Only master thread does this
      IF (TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Number of threads = ', NTHREADS
      END IF
      PRINT *, 'Thread ',TID,'is starting...'

!$OMP BARRIER

C     Do some work
      TOTAL = 0.0
!$OMP DO SCHEDULE(DYNAMIC,10)
      DO I=1, 1000000
        TOTAL = TOTAL + I * 1.0
      END DO

      WRITE(*,100) TID,TOTAL
 100  FORMAT('Thread',I2,' is done! Total= ',E12.6)

!$OMP END PARALLEL

      END
