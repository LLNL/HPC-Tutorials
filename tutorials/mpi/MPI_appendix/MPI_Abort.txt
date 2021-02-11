MPI_Abort(3)                          MPI                         MPI_Abort(3)



NAME
       MPI_Abort -  Terminates MPI execution environment

SYNOPSIS
       #include "mpi.h"
       int MPI_Abort( MPI_Comm comm, int errorcode )

INPUT PARAMETERS
       comm   - communicator of tasks to abort
       errorcode
              - error code to return to invoking environment


NOTES
       Terminates all MPI processes associated with the communicator comm ; in
       most systems (all to date), terminates all processes.


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       abort.c



                                  12/13/2001                      MPI_Abort(3)
