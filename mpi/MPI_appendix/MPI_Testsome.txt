MPI_Testsome(3)                       MPI                      MPI_Testsome(3)



NAME
       MPI_Testsome -  Tests for some given communications to complete

SYNOPSIS
       #include "mpi.h"
       int MPI_Testsome(
               int incount,
               MPI_Request array_of_requests[],
               int *outcount,
               int array_of_indices[],
               MPI_Status array_of_statuses[] )

INPUT PARAMETERS
       incount
              - length of array_of_requests (integer)
       array_of_requests
              - array of requests (array of handles)


OUTPUT PARAMETERS
       outcount
              - number of completed requests (integer)
       array_of_indices
              -  array of indices of operations that completed (array of inte-
              gers)
       array_of_statuses
              - array of status objects for operations that  completed  (array
              of Status).  May be MPI_STATUSES_IGNORE .



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
       MPI_ERR_IN_STATUS
              -  The  actual  error value is in the MPI_Status argument.  This
              error  class  is  returned  only  from  the  multiple-completion
              routines  ( MPI_Testall , MPI_Testany , MPI_Testsome , MPI_Wait-
              all , MPI_Waitany , and MPI_Waitsome ).  The field MPI_ERROR  in
              the  status argument contains the error value or MPI_SUCCESS (no
              error and complete) or  MPI_ERR_PENDING  to  indicate  that  the
              request  has  not  completed.  The MPI Standard does not specify
              what the result of the multiple completion routines is  when  an
              error occurs.  For example, in an MPI_WAITALL , does the routine
              wait for all requests to either fail or  complete,  or  does  it
              return  immediately  (with  the  MPI  definition of immediately,
              which means independent of  actions  of  other  MPI  processes)?
              MPICH  has  chosen  to  make  the return immediate (alternately,
              local in MPI terms), and to use the error class  MPI_ERR_PENDING
              (introduced in MPI 1.1) to indicate which requests have not com-
              pleted.  In most cases, only one request with an error  will  be
              detected  in  each  call  to  an MPI routine that tests multiple
              requests.  The requests that have not been processed (because an
              error  occured in one of the requests) will have their MPI_ERROR
              field marked with MPI_ERR_PENDING .



LOCATION
       testsome.c



                                   1/9/2003                    MPI_Testsome(3)
