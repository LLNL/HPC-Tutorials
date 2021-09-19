C******************************************************************************
C FILE: omp_mm.f
C DESCRIPTION:  
C   OpenMp Example - Matrix Multiply - Fortran Version 
C   Demonstrates a matrix multiply using OpenMP. Threads share row iterations
C   according to a predefined chunk size.
C AUTHOR: Blaise Barney
C LAST REVISED: 1/5/04 Blaise Barney
C******************************************************************************

      PROGRAM MATMULT

      INTEGER  NRA, NCA, NCB, TID, NTHREADS, I, J, K, CHUNK,
     +         OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
C     number of rows in matrix A 
      PARAMETER (NRA=62)
C     number of columns in matrix A
      PARAMETER (NCA=15)
C     number of columns in matrix B
      PARAMETER (NCB=7)

      REAL*8 A(NRA,NCA), B(NCA,NCB), C(NRA,NCB)

C     Set loop iteration chunk size 
      CHUNK = 10

C     Spawn a parallel region explicitly scoping all variables
!$OMP PARALLEL SHARED(A,B,C,NTHREADS,CHUNK) PRIVATE(TID,I,J,K)
      TID = OMP_GET_THREAD_NUM()
      IF (TID .EQ. 0) THEN
        NTHREADS = OMP_GET_NUM_THREADS()
        PRINT *, 'Starting matrix multiple example with', NTHREADS,
     +           'threads'
        PRINT *, 'Initializing matrices'
      END IF

C     Initialize matrices
!$OMP DO SCHEDULE(STATIC, CHUNK)
      DO 30 I=1, NRA
        DO 30 J=1, NCA
          A(I,J) = (I-1)+(J-1)
  30  CONTINUE
!$OMP DO SCHEDULE(STATIC, CHUNK)
      DO 40 I=1, NCA
        DO 40 J=1, NCB
          B(I,J) = (I-1)*(J-1)
  40  CONTINUE
!$OMP DO SCHEDULE(STATIC, CHUNK)
      DO 50 I=1, NRA
        DO 50 J=1, NCB
          C(I,J) = 0
  50  CONTINUE

C     Do matrix multiply sharing iterations on outer loop
C     Display who does which iterations for demonstration purposes
      PRINT *, 'Thread', TID, 'starting matrix multiply...'
!$OMP DO SCHEDULE(STATIC, CHUNK)
      DO 60 I=1, NRA
      PRINT *, 'Thread', TID, 'did row', I
        DO 60 J=1, NCB
          DO 60 K=1, NCA
            C(I,J) = C(I,J) + A(I,K) * B(K,J)
  60  CONTINUE

C     End of parallel region 
!$OMP END PARALLEL

C     Print results
      PRINT *, '******************************************************'
      PRINT *, 'Result Matrix:'
      DO 90 I=1, NRA
        DO 80 J=1, NCB
          WRITE(*,70) C(I,J)
  70      FORMAT(2x,f8.2,$)
  80      CONTINUE
          PRINT *, ' '
  90      CONTINUE
      PRINT *, '******************************************************'
      PRINT *, 'Done.'

      END
