MPI_Attr_put(3)                       MPI                      MPI_Attr_put(3)



NAME
       MPI_Attr_put -  Stores attribute value associated with a key

SYNOPSIS
       #include "mpi.h"
       int MPI_Attr_put ( MPI_Comm comm, int keyval, void *attr_value )

INPUT PARAMETERS
       comm   - communicator to which attribute will be attached (handle)
       keyval - key value, as returned by MPI_KEYVAL_CREATE (integer)
       attribute_val
              - attribute value


NOTES
       Values of the permanent attributes MPI_TAG_UB , MPI_HOST , MPI_IO , and
       MPI_WTIME_IS_GLOBAL may not be changed.

       The type of the attribute value depends on  whether  C  or  Fortran  is
       being  used.  In C, an attribute value is a pointer ( void * ); in For-
       tran, it is a single integer ( not a  pointer,  since  Fortran  has  no
       pointers  and  there are systems for which a pointer does not fit in an
       integer (e.g., any > 32 bit address system that uses 64 bits  for  For-
       tran DOUBLE PRECISION ).

       If an attribute is already present, the delete function (specified when
       the corresponding keyval was created) will be called.


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
       MPI_ERR_OTHER
              -  Other  error; the error code associated with this error indi-
              cates an attempt to use an invalue keyval.
       MPI_ERR_ARG
              - This error class is associated with an error code  that  indi-
              cates  that  an  attempt  was  made to free one of the permanent
              keys.


SEE ALSO
       MPI_Attr_get, MPI_Keyval_create, MPI_Attr_delete

LOCATION
       attr_putval.c



                                  11/14/2001                   MPI_Attr_put(3)
