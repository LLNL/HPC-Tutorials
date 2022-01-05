MPI_Errhandler_get(3)                 MPI                MPI_Errhandler_get(3)



NAME
       MPI_Errhandler_get -  Gets the error handler for a communicator

SYNOPSIS
       #include "mpi.h"
       int MPI_Errhandler_get( MPI_Comm comm, MPI_Errhandler *errhandler )

INPUT PARAMETER
       comm   - communicator to get the error handler from (handle)


OUTPUT PARAMETER
       errhandler
              - MPI error handler currently associated with communicator (han-
              dle)


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


NOTE ON IMPLEMENTATION
       The  MPI Standard was unclear on whether this routine required the user
       to call MPI_Errhandler_free once for each call made to this routine  in
       order  to  free  the  error  handler.  After some debate, the MPI Forum
       added an explicit statement that users are required to call MPI_Errhan-
       dler_free  when the return value from this routine is no longer needed.
       This behavior is similar to the other MPI routines for getting objects;
       for  example, MPI_Comm_group requires that the user call MPI_Group_free
       when the group returned by MPI_Comm_group is no longer needed.


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
       MPI_ERR_COMM
              -  Invalid communicator.  A common error is to use a null commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       errget.c



                                  11/14/2001             MPI_Errhandler_get(3)
