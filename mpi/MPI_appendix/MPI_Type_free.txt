MPI_Type_free(3)                      MPI                     MPI_Type_free(3)



NAME
       MPI_Type_free -  Frees the datatype

SYNOPSIS
       #include "mpi.h"
       int MPI_Type_free ( MPI_Datatype *datatype )

INPUT PARAMETER
       datatype
              - datatype that is freed (handle)


PREDEFINED TYPES
       The MPI standard states that (in Opaque Objects)
       MPI provides certain predefined opaque objects and predefined, static handles
       to these objects. Such objects may not be destroyed.


       Thus,  it  is an error to free a predefined datatype.  The same section
       makes it clear that it is an error to free a null datatype.


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
       MPI_ERR_TYPE
              - Invalid datatype argument.  May be an uncommitted MPI_Datatype
              (see MPI_Type_commit ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       type_free.c



                                  11/14/2001                  MPI_Type_free(3)
