MPI_Test(3)                           MPI                          MPI_Test(3)



NAME
       MPI_Test -  Tests for the completion of a send or receive

SYNOPSIS
       #include "mpi.h"
       int MPI_Test (
               MPI_Request  *request,
               int          *flag,
               MPI_Status   *status)

INPUT PARAMETER
       request
              - communication request (handle)


OUTPUT PARAMETER
       flag   - true if operation completed (logical)
       status - status object (Status).  May be MPI_STATUS_IGNORE .



NOTE ON STATUS FOR SEND OPERATIONS
       For  send  operations, the only use of status is for MPI_Test_cancelled
       or in the case that there is an error,  in  which  case  the  MPI_ERROR
       field of status will be set.


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
       MPI_ERR_REQUEST
              -  Invalid  MPI_Request  .   Either  null  or,  in the case of a
              MPI_Start or MPI_Startall , not a persistent request.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       test.c



                                   1/9/2003                        MPI_Test(3)
