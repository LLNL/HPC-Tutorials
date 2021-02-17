MPI_Error_string(3)                   MPI                  MPI_Error_string(3)



NAME
       MPI_Error_string -  Return a string for a given error code

SYNOPSIS
       #include "mpi.h"
       int MPI_Error_string( int errorcode, char *string, int *resultlen )

INPUT PARAMETERS
       errorcode
              - Error code returned by an MPI routine or an MPI error class


OUTPUT PARAMETER
       string - Text that corresponds to the errorcode
       resultlen
              - Length of string

              Notes:  Error codes are the values return by MPI routines (in C)
              or in the ierr argument (in Fortran).  These  can  be  converted
              into error classes with the routine MPI_Error_class .



NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       errorstring.c



                                  11/14/2001               MPI_Error_string(3)
