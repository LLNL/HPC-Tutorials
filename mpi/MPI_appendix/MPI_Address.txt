MPI_Address(3)                        MPI                       MPI_Address(3)



NAME
       MPI_Address -  Gets the address of a location in memory

SYNOPSIS
       #include "mpi.h"
       int MPI_Address( void *location, MPI_Aint *address)

INPUT PARAMETERS
       location
              - location in caller memory (choice)


OUTPUT PARAMETER
       address
              - address of location (integer)


NOTE
       This  routine  is  provided for both the Fortran and C programmers.  On
       many systems, the address returned by this routine will be the same  as
       produced by the C & operator, but this is not required in C and may not
       be true of systems with word- rather than byte-oriented instructions or
       systems with segmented address spaces.


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       address.c



                                  11/14/2001                    MPI_Address(3)
