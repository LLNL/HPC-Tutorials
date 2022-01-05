MPI_Allgatherv(3)                     MPI                    MPI_Allgatherv(3)



NAME
       MPI_Allgatherv -  Gathers data from all tasks and deliver it to all

SYNOPSIS
       #include "mpi.h"
       int MPI_Allgatherv ( void *sendbuf, int sendcount, MPI_Datatype sendtype,
                            void *recvbuf, int *recvcounts, int *displs,
                           MPI_Datatype recvtype, MPI_Comm comm )

INPUT PARAMETERS
       sendbuf
              - starting address of send buffer (choice)
       sendcount
              - number of elements in send buffer (integer)
       sendtype
              - data type of send buffer elements (handle)
       recvcounts
              -  integer array (of length group size) containing the number of
              elements that are received from each process
       displs - integer array (of length group size). Entry  i  specifies  the
              displacement (relative to recvbuf ) at which to place the incom-
              ing data from process i

       recvtype
              - data type of receive buffer elements (handle)
       comm   - communicator (handle)


OUTPUT PARAMETER
       recvbuf
              - address of receive buffer (choice)


NOTES
       The MPI standard (1.0 and 1.1) says that

       The jth block of data sent from each proess is received by  every  pro-
       cess and placed in the jth block of the buffer recvbuf .


       This is misleading; a better description is

       The  block  of data sent from the jth process is received by every pro-
       cess and placed in the jth block of the buffer recvbuf .


       This text was suggested by Rajeev Thakur.


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

       MPI_ERR_BUFFER
              - Invalid buffer pointer.  Usually a null buffer  where  one  is
              not valid.
       MPI_ERR_COUNT
              - Invalid count argument.  Count arguments must be non-negative;
              a count of zero is often valid.
       MPI_ERR_TYPE
              - Invalid datatype argument.  May be an uncommitted MPI_Datatype
              (see MPI_Type_commit ).

LOCATION
       allgatherv.c



                                  11/14/2001                 MPI_Allgatherv(3)
