C******************************************************************************
C FILE: omp_getEnvInfo.f
C DESCRIPTION:
C   OpenMP Example - Get Environment Information - Fortran Version
C   The master thread queries and prints selected environment information.
C AUTHOR: Blaise Barney  7/06
C LAST REVISED: 07/12/06
C******************************************************************************

      PROGRAM GETINFO
     
      INTEGER NTHREADS, TID, OMP_GET_NUM_THREADS,
     +  OMP_GET_THREAD_NUM, OMP_GET_NUM_PROCS, OMP_GET_MAX_THREADS,
     +  OMP_IN_PARALLEL, OMP_GET_DYNAMIC, OMP_GET_NESTED,
     +  PROCS, MAXT

C     These are for AIX compilations
C     INTEGER INPAR, DYNAMIC, NESTED
C     These are for non-AIX compilations
      LOGICAL INPAR, DYNAMIC, NESTED
  
C     Start parallel region
!$OMP PARALLEL PRIVATE(NTHREADS, TID)

C     Obtain thread number
      TID = OMP_GET_THREAD_NUM()

C     Only master thread does this
      IF (TID .EQ. 0) THEN

        PRINT *, 'Thread',tid,'getting environment information'

C     Get environment information
        PROCS = OMP_GET_NUM_PROCS() 
        NTHREADS = OMP_GET_NUM_THREADS()
        MAXT = OMP_GET_MAX_THREADS()
        INPAR = OMP_IN_PARALLEL()
        DYNAMIC = OMP_GET_DYNAMIC()
        NESTED = OMP_GET_NESTED()

C     Print environment information
    
        PRINT *, 'Number of processors = ', PROCS
        PRINT *, 'Number of threads = ', NTHREADS
        PRINT *, 'Max threads = ', MAXT
        PRINT *, 'In parallel? = ', INPAR
        PRINT *, 'Dynamic threads enabled? = ', DYNAMIC
        PRINT *, 'Nested parallelism supported? = ', NESTED

      END IF

C     Done
!$OMP END PARALLEL

      END

