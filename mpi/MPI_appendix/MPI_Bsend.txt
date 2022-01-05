MPI_Bsend(3)                          MPI                         MPI_Bsend(3)



NAME
       MPI_Bsend -  Basic send with user-specified buffering

SYNOPSIS
       #include "mpi.h"
       int MPI_Bsend(
               void *buf,
               int count,
               MPI_Datatype datatype,
               int dest,
               int tag,
               MPI_Comm comm )

INPUT PARAMETERS
       buf    - initial address of send buffer (choice)
       count  - number of elements in send buffer (nonnegative integer)
       datatype
              - datatype of each send buffer element (handle)
       dest   - rank of destination (integer)
       tag    - message tag (integer)
       comm   - communicator (handle)


NOTES
       This  send is provided as a convenience function; it allows the user to
       send messages without worring about where they  are  buffered  (because
       the user must have provided buffer space with MPI_Buffer_attach ).

       In deciding how much buffer space to allocate, remember that the buffer
       space is not available for reuse by subsequent MPI_Bsend s  unless  you
       are certain that the message has been received (not just that it should
       have been received).  For example, this code does not  allocate  enough
       buffer space
       MPI_Buffer_attach( b, n*sizeof(double) + MPI_BSEND_OVERHEAD );
       for (i=0; i<m; i++) {
       MPI_Bsend( buf, n, MPI_DOUBLE, ... );
       }

       because only enough buffer space is provided for a single send, and the
       loop may start a second MPI_Bsend before the first is done  making  use
       of the buffer.

       In C, you can force the messages to be delivered by
       MPI_Buffer_detach( &b, &n );
       MPI_Buffer_attach( b, n );

       (The  MPI_Buffer_detach  will  not complete until all buffered messages
       are delivered.)


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
       MPI_ERR_COMM
              - Invalid communicator.  A common error is to use a null  commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_COUNT
              - Invalid count argument.  Count arguments must be non-negative;
              a count of zero is often valid.
       MPI_ERR_TYPE
              - Invalid datatype argument.  May be an uncommitted MPI_Datatype
              (see MPI_Type_commit ).
       MPI_ERR_RANK
              -  Invalid  source  or  destination rank.  Ranks must be between
              zero and the size of the communicator  minus  one;  ranks  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_SOURCE .

       MPI_ERR_TAG
              - Invalid tag argument.  Tags must be non-negative;  tags  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_TAG .  The largest tag value is available through the
              the attribute MPI_TAG_UB .



SEE ALSO
       MPI_Buffer_attach, MPI_Ibsend, MPI_Bsend_init

LOCATION
       bsend.c



                                  11/14/2001                      MPI_Bsend(3)
