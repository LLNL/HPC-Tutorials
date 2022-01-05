MPI_Comm_dup(3)                       MPI                      MPI_Comm_dup(3)



NAME
       MPI_Comm_dup -  Duplicates an existing communicator with all its cached
       information

SYNOPSIS
       #include "mpi.h"
       int MPI_Comm_dup (
               MPI_Comm comm,
               MPI_Comm *comm_out )

INPUT PARAMETER
       comm   - communicator (handle)


OUTPUT PARAMETER
       newcomm
              - A new communicator over the same group as comm but with a  new
              context. See notes.  (handle)


NOTES
       This routine is used to create a new communicator that has a new commu-
       nication context but contains the same group of processes as the  input
       communicator.  Since all MPI communication is performed within a commu-
       nicator (specifies as the group of processes plus  the  context),  this
       routine  provides an effective way to create a private communicator for
       use by a software module or library.  In particular, no library routine
       should  use MPI_COMM_WORLD as the communicator; instead, a duplicate of
       a user-specified communicator should always be used.  For more informa-
       tion, see Using MPI, 2nd edition.

       Because  this routine essentially produces a copy of a communicator, it
       also copies any attributes that have been defined on the input communi-
       cator, using the attribute copy function specified by the copy_function
       argument to MPI_Keyval_create .  This is particularly  useful  for  (a)
       attributes that describe some property of the group associated with the
       communicator, such as its interconnection topology and  (b)  communica-
       tors  that  are  given back to the user; the attibutes in this case can
       track subsequent MPI_Comm_dup operations on this communicator.


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
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.


SEE ALSO
       MPI_Comm_free, MPI_Keyval_create, MPI_Attr_set, MPI_Attr_delete


LOCATION
       comm_dup.c



                                  11/14/2001                   MPI_Comm_dup(3)
