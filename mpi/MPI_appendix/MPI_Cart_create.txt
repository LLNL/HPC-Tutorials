MPI_Cart_create(3)                    MPI                   MPI_Cart_create(3)



NAME
       MPI_Cart_create  -  Makes a new communicator to which topology informa-
       tion has been attached

SYNOPSIS
       #include "mpi.h"
       int MPI_Cart_create ( MPI_Comm comm_old, int ndims, int *dims, int *periods,
                            int reorder, MPI_Comm *comm_cart )

INPUT PARAMETERS
       comm_old
              - input communicator (handle)
       ndims  - number of dimensions of cartesian grid (integer)
       dims   - integer array of size ndims specifying the number of processes
              in each dimension
       periods
              -  logical  array  of  size ndims specifying whether the grid is
              periodic (true) or not (false) in each dimension
       reorder
              - ranking may be reordered (true) or not (false) (logical)


OUTPUT PARAMETER
       comm_cart
              - communicator with new cartesian topology (handle)


ALGORITHM
       We ignore reorder info currently.


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


ERRORS
       All  MPI  routines  (except  MPI_Wtime  and MPI_Wtick ) return an error
       value; C routines as the value of the function and Fortran routines  in
       the last argument.  Before the value is returned, the current MPI error
       handler is called.  By default, this error handler aborts the MPI  job.
       The  error  handler may be changed with MPI_Errhandler_set ; the prede-
       fined error handler MPI_ERRORS_RETURN may be used to cause error values
       to  be  returned.  Note that MPI does not guarentee that an MPI program
       can continue past an error.

       MPI_SUCCESS
              - No error; MPI routine completed successfully.
       MPI_ERR_TOPOLOGY
              - Invalid topology.  Either there is no topology associated with
              this communicator, or it is not the correct type (e.g., MPI_CART
              when expecting MPI_GRAPH ).
       MPI_ERR_DIMS
              - Illegal dimension argument.  A dimension argument is  null  or
              its length is less than or equal to zero.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       cart_create.c



                                  11/14/2001                MPI_Cart_create(3)
