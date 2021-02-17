MPI_Initialized(3)                    MPI                   MPI_Initialized(3)



NAME
       MPI_Initialized -  Indicates whether MPI_Init has been called.

SYNOPSIS
       #include "mpi.h"
       int MPI_Initialized( int *flag )

OUTPUT PARAMETER
       flag   -  Flag is true if MPI_Init has been called and false otherwise.


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       initialize.c



                                  11/14/2001                MPI_Initialized(3)
