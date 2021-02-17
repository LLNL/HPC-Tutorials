MPI_Startall(3)                       MPI                      MPI_Startall(3)



NAME
       MPI_Startall -  Starts a collection of requests

SYNOPSIS
       #include "mpi.h"
       int MPI_Startall( int count, MPI_Request array_of_requests[] )

INPUT PARAMETERS
       count  - list length (integer)
       array_of_requests
              - array of requests (array of handle)


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       startall.c



                                  11/14/2001                   MPI_Startall(3)
