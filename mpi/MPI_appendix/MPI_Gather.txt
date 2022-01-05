MPI_Gather(3)                         MPI                        MPI_Gather(3)



NAME
       MPI_Gather -  Gathers together values from a group of processes

SYNOPSIS
       #include "mpi.h"
       int MPI_Gather ( void *sendbuf, int sendcnt, MPI_Datatype sendtype,
                       void *recvbuf, int recvcount, MPI_Datatype recvtype,
                       int root, MPI_Comm comm )

INPUT PARAMETERS
       sendbuf
              - starting address of send buffer (choice)
       sendcount
              - number of elements in send buffer (integer)
       sendtype
              - data type of send buffer elements (handle)
       recvcount
              -  number  of elements for any single receive (integer, signifi-
              cant only at root)
       recvtype
              - data type of recv buffer elements (significant only  at  root)
              (handle)
       root   - rank of receiving process (integer)
       comm   - communicator (handle)


OUTPUT PARAMETER
       recvbuf
              - address of receive buffer (choice, significant only at root )


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
       MPI_ERR_COMM
              -  Invalid communicator.  A common error is to use a null commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_COUNT
              - Invalid count argument.  Count arguments must be non-negative;
              a count of zero is often valid.
       MPI_ERR_TYPE
              - Invalid datatype argument.  May be an uncommitted MPI_Datatype
              (see MPI_Type_commit ).
       MPI_ERR_BUFFER
              - Invalid buffer pointer.  Usually a null buffer  where  one  is
              not valid.

LOCATION
       gather.c



                                  11/14/2001                     MPI_Gather(3)
