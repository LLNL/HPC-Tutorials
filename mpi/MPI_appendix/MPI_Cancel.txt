MPI_Cancel(3)                         MPI                        MPI_Cancel(3)



NAME
       MPI_Cancel -  Cancels a communication request

SYNOPSIS
       #include "mpi.h"
       int MPI_Cancel( MPI_Request *request )

INPUT PARAMETER
       request
              - communication request (handle)


NOTE
       Cancel  has  only  been implemented for receive requests; it is a no-op
       for send requests.  The primary expected use of MPI_Cancel is in multi-
       buffering  schemes,  where  speculative  MPI_Irecvs are made.  When the
       computation completes, some of these receive requests may remain; using
       MPI_Cancel allows the user to cancel these unsatisfied requests.

       Cancelling  a  send  operation  is  much  more difficult, in large part
       because the send will usually  be  at  least  partially  complete  (the
       information  on  the tag, size, and source are usually sent immediately
       to the destination).  As of version 1.2.0, MPICH supports cancelling of
       sends.   Users are advised that cancelling a send, while a local opera-
       tion (as defined by the MPI standard), is likely to be expensive  (usu-
       ally generating one or more internal messages).


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


NULL HANDLES
       The MPI 1.1 specification, in the section on opaque objects, explicitly

DISALLOWS FREEING A NULL COMMUNICATOR. THE TEXT FROM THE STANDARD IS
       A null handle argument is an erroneous IN argument in MPI calls, unless an
       exception is explicitly stated in the text that defines the function. Such
       exception is allowed for handles to request objects in Wait and Test calls
       (sections Communication Completion and Multiple Completions ). Otherwise, a
       null handle can only be passed to a function that allocates a new object and
       returns a reference to it in the handle.



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
       MPI_ERR_REQUEST
              - Invalid MPI_Request .  Either  null  or,  in  the  case  of  a
              MPI_Start or MPI_Startall , not a persistent request.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       cancel.c



                                  11/14/2001                     MPI_Cancel(3)
