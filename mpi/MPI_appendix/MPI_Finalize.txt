MPI_Finalize(3)                       MPI                      MPI_Finalize(3)



NAME
       MPI_Finalize -  Terminates MPI execution environment

SYNOPSIS
       #include "mpi.h"
       int MPI_Finalize()

NOTES
       All  processes  must  call  this routine before exiting.  The number of
       processes running after this routine is called is undefined; it is best
       not  to perform much more than a return rc after calling MPI_Finalize .



NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       finalize.c



                                   4/9/2002                    MPI_Finalize(3)
