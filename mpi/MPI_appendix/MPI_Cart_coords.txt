MPI_Cart_coords(3)                    MPI                   MPI_Cart_coords(3)



NAME
       MPI_Cart_coords  -   Determines  process  coords  in cartesian topology
       given rank in group

SYNOPSIS
       #include "mpi.h"
       int MPI_Cart_coords ( MPI_Comm comm, int rank, int maxdims, int *coords )

INPUT PARAMETERS
       comm   - communicator with cartesian structure (handle)
       rank   - rank of a process within group of comm (integer)
       maxdims
              - length of vector coords in the calling program (integer)


OUTPUT PARAMETER
       coords - integer array (of size ndims ) containing the Cartesian  coor-
              dinates of specified process (integer)


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


ERRORS
       All MPI routines (except MPI_Wtime and  MPI_Wtick  )  return  an  error
       value;  C routines as the value of the function and Fortran routines in
       the last argument.  Before the value is returned, the current MPI error
       handler  is called.  By default, this error handler aborts the MPI job.
       The error handler may be changed with MPI_Errhandler_set ;  the  prede-
       fined error handler MPI_ERRORS_RETURN may be used to cause error values
       to be returned.  Note that MPI does not guarentee that an  MPI  program
       can continue past an error.

       MPI_SUCCESS
              - No error; MPI routine completed successfully.
       MPI_ERR_TOPOLOGY
              - Invalid topology.  Either there is no topology associated with
              this communicator, or it is not the correct type (e.g., MPI_CART
              when expecting MPI_GRAPH ).
       MPI_ERR_RANK
              -  Invalid  source  or  destination rank.  Ranks must be between
              zero and the size of the communicator  minus  one;  ranks  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_SOURCE .

       MPI_ERR_DIMS
              - Illegal dimension argument.  A dimension argument is  null  or
              its length is less than or equal to zero.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).


LOCATION
       cart_coords.c



                                   3/28/2002                MPI_Cart_coords(3)
