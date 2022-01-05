MPI_Bcast(3)                          MPI                         MPI_Bcast(3)



NAME
       MPI_Bcast  -  Broadcasts a message from the process with rank "root" to
       all other processes of the group.

SYNOPSIS
       #include "mpi.h"
       int MPI_Bcast ( void *buffer, int count, MPI_Datatype datatype, int root,
                      MPI_Comm comm )

INPUT/OUTPUT PARAMETERS
       buffer - starting address of buffer (choice)
       count  - number of entries in buffer (integer)
       datatype
              - data type of buffer (handle)
       root   - rank of broadcast root (integer)
       comm   - communicator (handle)


ALGORITHM
       If the underlying device does not take  responsibility,  this  function
       uses  a  tree-like algorithm to broadcast the message to blocks of pro-
       cesses.  A linear algorithm is then used to broadcast the message  from
       the    first   process   in   a   block   to   all   other   processes.
       MPIR_BCAST_BLOCK_SIZE determines the size of blocks.  If this is set to
       1, then this function is equivalent to using a pure tree algorithm.  If
       it is set to the size of the group or greater,  it  is  a  pure  linear
       algorithm.   The  value  should be adjusted to determine the most effi-
       cient value on different machines.


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
       MPI_ERR_BUFFER
              -  Invalid  buffer  pointer.  Usually a null buffer where one is
              not valid.
       MPI_ERR_ROOT
              - Invalid root.  The root must be specified as  a  rank  in  the
              communicator.   Ranks  must  be between zero and the size of the
              communicator minus one.

LOCATION
       bcast.c



                                  11/14/2001                      MPI_Bcast(3)
