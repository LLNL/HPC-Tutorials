MPI_Errhandler_create(3)              MPI             MPI_Errhandler_create(3)



NAME
       MPI_Errhandler_create -  Creates an MPI-style errorhandler

SYNOPSIS
       #include "mpi.h"
       int MPI_Errhandler_create(
               MPI_Handler_function *function,
               MPI_Errhandler       *errhandler)

INPUT PARAMETER
       function
              - user defined error handling procedure


OUTPUT PARAMETER
       errhandler
              - MPI error handler (handle)


NOTES
       The  MPI  Standard  states  that  an implementation may make the output
       value (errhandler) simply the address of the  function.   However,  the
       action  of  MPI_Errhandler_free  makes  this  impossible,  since  it is
       required to set the value of the argument to MPI_ERRHANDLER_NULL .   In
       addition,  the actual error handler must remain until all communicators
       that use it are freed.


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
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.

LOCATION
       errcreate.c



                                  11/14/2001          MPI_Errhandler_create(3)
