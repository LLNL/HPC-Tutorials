MPI_Keyval_free(3)                    MPI                   MPI_Keyval_free(3)



NAME
       MPI_Keyval_free -  Frees attribute key for communicator cache attribute

SYNOPSIS
       #include "mpi.h"
       int MPI_Keyval_free ( int *keyval )

INPUT PARAMETER
       keyval - Frees the integer key value (integer)


NOTE
       Key values are global (they can be used with any and all communicators)


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
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).
       MPI_ERR_ARG
              - This error class is associated with an error code  that  indi-
              cates  that  an  attempt  was  made to free one of the permanent
              keys.


SEE ALSO
       MPI_Keyval_create

LOCATION
       keyval_free.c



                                   6/12/2002                MPI_Keyval_free(3)
