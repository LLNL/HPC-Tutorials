MPI_Buffer_attach(3)                  MPI                 MPI_Buffer_attach(3)



NAME
       MPI_Buffer_attach -  Attaches a user-defined buffer for sending

SYNOPSIS
       #include "mpi.h"
       int MPI_Buffer_attach( void *buffer, int size )

INPUT PARAMETERS
       buffer - initial buffer address (choice)
       size   - buffer size, in bytes (integer)


NOTES
       The size given should be the sum of the sizes of all outstanding Bsends
       that you intend to have, plus a few hundred bytes for each  Bsend  that
       you  do.   For  the  purposes  of  calculating  size,  you  should  use
       MPI_Pack_size .

       In other words, in the code
       MPI_Buffer_attach( buffer, size );
       MPI_Bsend( ..., count=20, datatype=type1,  ... );
       .
       .
       .
       MPI_Bsend( ..., count=40, datatype=type2, ... );

       the value of size in the MPI_Buffer_attach call should be greater  than
       the value computed by
       MPI_Pack_size( 20, type1, comm, &s1 );
       MPI_Pack_size( 40, type2, comm, &s2 );
       size = s1 + s2 + 2 * MPI_BSEND_OVERHEAD;

       The  MPI_BSEND_OVERHEAD  gives  the maximum amount of space that may be
       used in the buffer for use by the BSEND routines in using  the  buffer.
       This value is in mpi.h (for C) and mpif.h (for Fortran).


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
       MPI_ERR_BUFFER
              -  Invalid  buffer  pointer.  Usually a null buffer where one is
              not valid.
       MPI_ERR_INTERN
              - An internal error has been detected.  This is  fatal.   Please
              send a bug report to mpi-bugs@mcs.anl.gov .



SEE ALSO
       MPI_Buffer_detach, MPI_Bsend

LOCATION
       bufattach.c



                                  11/14/2001              MPI_Buffer_attach(3)
