MPI_Type_vector(3)                    MPI                   MPI_Type_vector(3)



NAME
       MPI_Type_vector -  Creates a vector (strided) datatype

SYNOPSIS
       #include "mpi.h"
       int MPI_Type_vector(
               int count,
               int blocklen,
               int stride,
               MPI_Datatype old_type,
               MPI_Datatype *newtype )

INPUT PARAMETERS
       count  - number of blocks (nonnegative integer)
       blocklength
              - number of elements in each block (nonnegative integer)
       stride - number of elements between start of each block (integer)
       oldtype
              - old datatype (handle)


OUTPUT PARAMETER
       newtype
              - new datatype (handle)


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       type_vec.c



                                  11/14/2001                MPI_Type_vector(3)
