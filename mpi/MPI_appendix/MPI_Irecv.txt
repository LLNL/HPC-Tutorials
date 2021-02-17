MPI_Irecv(3)                          MPI                         MPI_Irecv(3)



NAME
       MPI_Irecv -  Begins a nonblocking receive

SYNOPSIS
       #include "mpi.h"
       int MPI_Irecv( void *buf, int count, MPI_Datatype datatype, int source,
                      int tag, MPI_Comm comm, MPI_Request *request )

INPUT PARAMETERS
       buf    - initial address of receive buffer (choice)
       count  - number of elements in receive buffer (integer)
       datatype
              - datatype of each receive buffer element (handle)
       source - rank of source (integer)
       tag    - message tag (integer)
       comm   - communicator (handle)


OUTPUT PARAMETER
       request
              - communication request (handle)


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       irecv.c



                                  11/14/2001                      MPI_Irecv(3)
