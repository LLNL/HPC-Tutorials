MPI_Request_free(3)                   MPI                  MPI_Request_free(3)



NAME
       MPI_Request_free -  Frees a communication request object

SYNOPSIS
       #include "mpi.h"
       int MPI_Request_free( MPI_Request *request )

INPUT PARAMETER
       request
              - communication request (handle)


NOTES
       This  routine is normally used to free persistent requests created with
       either MPI_Recv_init or MPI_Send_init and friends.  However, it can  be
       used to free a request created with MPI_Irecv or MPI_Isend and friends;
       in that case the use can not use the test/wait routines on the request.

       It  is  permitted  to free an active request.  However, once freed, you
       can not use the request in a wait or test routine (e.g., MPI_Wait ).


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
       MPI_ERR_REQUEST
              - Invalid MPI_Request .  Either  null  or,  in  the  case  of  a
              MPI_Start or MPI_Startall , not a persistent request.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).


SEE ALSO
       also: MPI_Isend, MPI_Irecv, MPI_Issend, MPI_Ibsend, MPI_Irsend,
       MPI_Recv_init, MPI_Send_init, MPI_Ssend_init, MPI_Rsend_init, MPI_Wait,
       MPI_Test, MPI_Waitall, MPI_Waitany, MPI_Waitsome, MPI_Testall, MPI_Tes-
       tany, MPI_Testsome

LOCATION
       commreq_free.c



                                  11/14/2001               MPI_Request_free(3)
