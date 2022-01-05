MPI_Cart_sub(3)                       MPI                      MPI_Cart_sub(3)



NAME
       MPI_Cart_sub  -   Partitions  a communicator into subgroups which  form
       lower-dimensional cartesian subgrids

SYNOPSIS
       #include "mpi.h"
       int MPI_Cart_sub ( MPI_Comm comm, int *remain_dims, MPI_Comm *comm_new )

INPUT PARAMETERS
       comm   - communicator with cartesian structure (handle)
       remain_dims
              - the i th entry of  remain_dims  specifies  whether  the  i  th
              dimension  is  kept  in the subgrid (true) or is dropped (false)
              (logical vector)


OUTPUT PARAMETER
       newcomm
              - communicator containing the subgrid that includes the  calling
              process (handle)


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
       MPI_ERR_COMM
              -  Invalid communicator.  A common error is to use a null commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       cart_sub.c



                                  11/14/2001                   MPI_Cart_sub(3)
