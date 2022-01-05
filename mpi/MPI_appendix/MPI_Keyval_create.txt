MPI_Keyval_create(3)                  MPI                 MPI_Keyval_create(3)



NAME
       MPI_Keyval_create -  Generates a new attribute key

SYNOPSIS
       #include "mpi.h"
       int MPI_Keyval_create (
               MPI_Copy_function *copy_fn,
               MPI_Delete_function *delete_fn,
               int *keyval,
               void *extra_state )

INPUT PARAMETERS
       copy_fn
              - Copy callback function for keyval

       delete_fn
              - Delete callback function for keyval

       extra_state
              - Extra state for callback functions


OUTPUT PARAMETER
       keyval - key value for future access (integer)


NOTES
       Key values are global (available for any and all communicators).

       There  are  subtle  differences between C and Fortran that require that
       the copy_fn be written in the same language that  MPI_Keyval_create  is
       called  from.   This  should not be a problem for most users; only pro-
       gramers using both Fortran and C in the same program need  to  be  sure
       that they follow this rule.


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
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       keyvalcreate.c



                                  11/14/2001              MPI_Keyval_create(3)
