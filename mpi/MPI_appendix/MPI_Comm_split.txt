MPI_Comm_split(3)                     MPI                    MPI_Comm_split(3)



NAME
       MPI_Comm_split -  Creates new communicators based on colors and keys

SYNOPSIS
       #include "mpi.h"
       int MPI_Comm_split ( MPI_Comm comm, int color, int key, MPI_Comm *comm_out )

INPUT PARAMETERS
       comm   - communicator (handle)
       color  - control of subset assignment (nonnegative integer).  Processes
              with the same color are in the same new communicator
       key    - control of rank assigment (integer)


OUTPUT PARAMETER
       newcomm
              - new communicator (handle)


NOTES
       The color must be non-negative or MPI_UNDEFINED .



NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


ALGORITHM
       The current algorithm used has quite a few (read: a lot of) inefficien-
       cies that can be removed.  Here is what we do for now

       1) A table is built of colors, and keys (has a next field also).
       2) The tables of all processes are merged using
       MPI_Allreduce
       .
       3) Two contexts are allocated for all the comms to be created.  These
       same two contexts can be used for all created communicators since
       the communicators will not overlap.
       4) If the local process has a color of
       MPI_UNDEFINED
       , it can return
       a
       NULL
       comm.
       5) The table entries that match the local process color are sorted
       by key/rank.
       6) A group is created from the sorted list and a communicator is created
       with this group and the previously allocated contexts.



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
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.


SEE ALSO
       MPI_Comm_free

LOCATION
       comm_split.c



                                   8/29/2002                 MPI_Comm_split(3)
