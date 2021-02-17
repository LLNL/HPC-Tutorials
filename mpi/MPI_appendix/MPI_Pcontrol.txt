MPI_Pcontrol(3)                       MPI                      MPI_Pcontrol(3)



NAME
       MPI_Pcontrol -  Controls profiling

SYNOPSIS
       #include "mpi.h"
       int MPI_Pcontrol( int level )

INPUT PARAMETERS
       level  - Profiling level


NOTES
       This  routine  provides  a common interface for profiling control.  The
       interpretation of level and any other arguments is left to the  profil-
       ing library.


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       pcontrol.c



                                  11/14/2001                   MPI_Pcontrol(3)
