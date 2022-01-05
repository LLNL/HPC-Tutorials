MPI_Testany(3)                        MPI                       MPI_Testany(3)



NAME
       MPI_Testany  -  Tests for completion of any previdously initiated  com-
       munication

SYNOPSIS
       #include "mpi.h"
       int MPI_Testany(
               int count,
               MPI_Request array_of_requests[],
               int *index, int *flag,
               MPI_Status *status )

INPUT PARAMETERS
       count  - list length (integer)
       array_of_requests
              - array of requests (array of handles)


OUTPUT PARAMETERS
       index  - index of operation that completed, or  MPI_UNDEFINED  if  none
              completed (integer)
       flag   - true if one of the operations is complete (logical)
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

LOCATION
       testany.c



                                   1/9/2003                     MPI_Testany(3)
