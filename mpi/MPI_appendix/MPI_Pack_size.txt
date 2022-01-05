MPI_Pack_size(3)                      MPI                     MPI_Pack_size(3)



NAME
       MPI_Pack_size  -  Returns the upper bound on the amount of space needed
       to pack a message

SYNOPSIS
       #include "mpi.h"
       int MPI_Pack_size ( int incount, MPI_Datatype datatype, MPI_Comm comm,
                          int *size )

INPUT PARAMETERS
       incount
              - count argument to packing call (integer)
       datatype
              - datatype argument to packing call (handle)
       comm   - communicator argument to packing call (handle)


OUTPUT PARAMETER
       size   - upper bound on size of packed message, in bytes (integer)


NOTES
       The MPI standard document describes this in terms of MPI_Pack , but  it
       applies  to  both MPI_Pack and MPI_Unpack .  That is, the value size is
       the maximum that is needed by either MPI_Pack or MPI_Unpack .



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
       MPI_ERR_TYPE
              - Invalid datatype argument.  May be an uncommitted MPI_Datatype
              (see MPI_Type_commit ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).


LOCATION
       pack_size.c



                                  11/14/2001                  MPI_Pack_size(3)
