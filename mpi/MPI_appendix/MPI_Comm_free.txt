MPI_Comm_free(3)                      MPI                     MPI_Comm_free(3)



NAME
       MPI_Comm_free -  Marks the communicator object for deallocation

SYNOPSIS
       #include "mpi.h"
       int MPI_Comm_free ( MPI_Comm *commp )

INPUT PARAMETER
       comm   - communicator to be destroyed (handle)


NULL HANDLES
       The MPI 1.1 specification, in the section on opaque objects, explicitly

DISALLOWS FREEING A NULL COMMUNICATOR. THE TEXT FROM THE STANDARD IS
       A null handle argument is an erroneous IN argument in MPI calls, unless an
       exception is explicitly stated in the text that defines the function. Such
       exception is allowed for handles to request objects in Wait and Test calls
       (sections Communication Completion and Multiple Completions ). Otherwise, a
       null handle can only be passed to a function that allocates a new object and
       returns a reference to it in the handle.



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
       MPI_ERR_COMM
              - Invalid communicator.  A common error is to use a null  commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       comm_free.c



                                  10/31/2002                  MPI_Comm_free(3)
