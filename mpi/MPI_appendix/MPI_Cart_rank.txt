MPI_Cart_rank(3)                      MPI                     MPI_Cart_rank(3)



NAME
       MPI_Cart_rank  -   Determines process rank in communicator given Carte-
       sian location

SYNOPSIS
       #include "mpi.h"
       int MPI_Cart_rank (
               MPI_Comm comm,
               int *coords,
               int *rank )

INPUT PARAMETERS
       comm   - communicator with cartesian structure (handle)
       coords - integer array (of size ndims ) specifying the cartesian  coor-
              dinates of a process


OUTPUT PARAMETER
       rank   - rank of specified process (integer)


NOTES
       Out-of-range  coordinates  are  erroneous  for non-periodic dimensions.
       Versions of MPICH before 1.2.2 returned MPI_PROC_NULL for the  rank  in
       this case.


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

       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       cart_rank.c



                                  11/14/2001                  MPI_Cart_rank(3)
