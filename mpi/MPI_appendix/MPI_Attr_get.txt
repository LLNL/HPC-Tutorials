MPI_Attr_get(3)                       MPI                      MPI_Attr_get(3)



NAME
       MPI_Attr_get -  Retrieves attribute value by key

SYNOPSIS
       #include "mpi.h"
       int MPI_Attr_get (
               MPI_Comm comm,
               int keyval,
               void *attr_value,
               int *flag )

INPUT PARAMETERS
       comm   - communicator to which attribute is attached (handle)
       keyval - key value (integer)


OUTPUT PARAMETERS
       attr_value
              - attribute value, unless flag = false
       flag   -  true  if  an  attribute  value  was  extracted;   false if no
              attribute is associated with the key


NOTES
       Attributes must be extracted  from  the  same  language  as  they  were
       inserted  in  with  MPI_ATTR_PUT  .   The notes for C and Fortran below
       explain why.


NOTES FOR C
       Even though the attr_value arguement is declared as  void  *  ,  it  is
       really  the  address of a void pointer.  See the rationale in the stan-
       dard for more details.


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

       The  attr_value  in  Fortran  is  a pointer to a Fortran integer, not a
       pointer to a void * .



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
       MPI_ERR_OTHER
              - Other error; the error code associated with this  error  indi-
              cates an attempt to use an invalue keyval.

LOCATION
       attr_getval.c



                                  11/14/2001                   MPI_Attr_get(3)
