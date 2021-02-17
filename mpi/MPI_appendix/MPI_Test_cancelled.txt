MPI_Test_cancelled(3)                 MPI                MPI_Test_cancelled(3)



NAME
       MPI_Test_cancelled -  Tests to see if a request was cancelled

SYNOPSIS
       #include "mpi.h"
       int MPI_Test_cancelled(
               MPI_Status *status,
               int        *flag)

INPUT PARAMETER
       status - status object (Status)


OUTPUT PARAMETER
       flag   - (logical)


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       testcancel.c



                                  11/14/2001             MPI_Test_cancelled(3)
