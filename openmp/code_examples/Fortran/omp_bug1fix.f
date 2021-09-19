C******************************************************************************
C FILE: omp_bug1fix.f
C DESCRIPTION:
C   This is a corrected version of the omp_bug1fix.f example. Corrections
C   include removing all statements between the PARALLEL DO construct and
C   the actual DO loop, and introducing logic to preserve the ability to 
C   query a thread's id and print it from inside the DO loop.
C AUTHOR: Blaise Barney  5/99
C LAST REVISED:
C******************************************************************************

      PROGRAM WORKSHARE4

      INTEGER TID, OMP_GET_THREAD_NUM, N, I, CHUNKSIZE, CHUNK
      PARAMETER (N=50)
      PARAMETER (CHUNKSIZE=5) 
      REAL A(N), B(N), C(N)
      CHARACTER FIRST_TIME

!     Some initializations
      DO I = 1, N
        A(I) = I * 1.0
        B(I) = A(I)
      ENDDO
      CHUNK = CHUNKSIZE
      FIRST_TIME = 'Y'
            
!$OMP  PARALLEL DO SHARED(A,B,C,CHUNK) 
!$OMP& PRIVATE(I,TID) 
!$OMP& SCHEDULE(STATIC,CHUNK)
!$OMP& FIRSTPRIVATE(FIRST_TIME) 

      DO I = 1, N
         IF (FIRST_TIME .EQ. 'Y') THEN
            TID = OMP_GET_THREAD_NUM()
            FIRST_TIME = 'N'
         ENDIF
         C(I) = A(I) + B(I)
         PRINT *,'TID= ',TID,'I= ',I,'C(I)= ',C(I)
      ENDDO

!$OMP  END PARALLEL DO

      END

