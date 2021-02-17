MPI_Dims_create(3)                    MPI                   MPI_Dims_create(3)



NAME
       MPI_Dims_create -  Creates a division of processors in a cartesian grid

SYNOPSIS
       #include "mpi.h"
       int MPI_Dims_create(
               int nnodes,
               int ndims,
               int *dims)

INPUT PARAMETERS
       nnodes - number of nodes in a grid (integer)
       ndims  - number of cartesian dimensions (integer)


IN/OUT PARAMETER
       dims   - integer array of size ndims specifying the number of nodes  in
              each dimension


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       dims_create.c



                                   4/24/2002                MPI_Dims_create(3)
