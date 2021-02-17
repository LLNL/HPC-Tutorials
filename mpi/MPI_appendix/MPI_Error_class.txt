MPI_Error_class(3)                    MPI                   MPI_Error_class(3)



NAME
       MPI_Error_class -  Converts an error code into an error class

SYNOPSIS
       #include "mpi.h"
       int MPI_Error_class(
               int errorcode,
               int *errorclass)

INPUT PARAMETER
       errorcode
              - Error code returned by an MPI routine


OUTPUT PARAMETER
       errorclass
              - Error class associated with errorcode



NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       errclass.c



                                  11/14/2001                MPI_Error_class(3)
