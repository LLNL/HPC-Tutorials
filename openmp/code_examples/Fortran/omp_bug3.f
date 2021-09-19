C******************************************************************************
C FILE: omp_bug3.f
C DESCRIPTION:
C   Run time bug
C AUTHOR: Blaise Barney  01/09/04
C LAST REVISED: 06/28/05
C******************************************************************************

      PROGRAM BUG3

      INTEGER N, I, NTHREADS, TID, SECTION, OMP_GET_NUM_THREADS, 
     +        OMP_GET_THREAD_NUM
      PARAMETER (N=50)
      REAL A(N), B(N), C(N)

C     Some initializations
      DO I = 1, N
        A(I) = I * 1.0
        B(I) = A(I)
      ENDDO

!$OMP PARALLEL PRIVATE(C,I,TID,SECTION)
      TID = OMP_GET_THREAD_NUM()
      IF (TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Number of threads = ', NTHREADS
      END IF

C     Use barriers for clean output
!$OMP BARRIER
      PRINT *, 'Thread ',TID,' starting...'
!$OMP BARRIER

!$OMP SECTIONS
!$OMP SECTION
      SECTION = 1
      DO I = 1, N
         C(I) = A(I) * B(I)
      ENDDO
      CALL PRINT_RESULTS(C, TID, SECTION)

!$OMP SECTION
      SECTION = 2
      DO I = 1, N
         C(I) = A(I) + B(I)
      ENDDO
      CALL PRINT_RESULTS(C, TID, SECTION)

!$OMP END SECTIONS 

C     Use barrier for clean output
!$OMP BARRIER
      PRINT *, 'Thread',tid,' exiting...'

!$OMP END PARALLEL

      END



      SUBROUTINE PRINT_RESULTS(C, TID, SECTION)
      INTEGER TID, SECTION, N, I, J
      PARAMETER (N=50)
      REAL C(N)

      J = 1
C     Use critical for clean output
!$OMP CRITICAL
      PRINT *, ' '
      PRINT *, 'Thread',TID,' did section',SECTION
      DO I=1, N
        WRITE(*,100) C(I)
 100    FORMAT(E12.6,$)
        J = J + 1
        IF (J .EQ. 6) THEN
          PRINT *, ' '
          J = 1
        END IF
      END DO
      PRINT *, ' '
!$OMP END CRITICAL

!$OMP BARRIER
      PRINT *,'Thread',TID,' done and synchronized'

      END SUBROUTINE PRINT_RESULTS
