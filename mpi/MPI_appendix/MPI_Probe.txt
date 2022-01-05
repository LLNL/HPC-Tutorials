MPI_Probe(3)                          MPI                         MPI_Probe(3)



NAME
       MPI_Probe -  Blocking test for a message

SYNOPSIS
       #include "mpi.h"
       int MPI_Probe( int source, int tag, MPI_Comm comm, MPI_Status *status )

INPUT PARAMETERS
       source - source rank, or MPI_ANY_SOURCE (integer)
       tag    - tag value or MPI_ANY_TAG (integer)
       comm   - communicator (handle)


OUTPUT PARAMETER
       status - status object (Status)


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
       MPI_ERR_TAG
              - Invalid tag argument.  Tags must be non-negative;  tags  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_TAG .  The largest tag value is available through the
              the attribute MPI_TAG_UB .

       MPI_ERR_RANK
              -  Invalid  source  or  destination rank.  Ranks must be between
              zero and the size of the communicator  minus  one;  ranks  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_SOURCE .


LOCATION
       probe.c



                                   12/7/2004                      MPI_Probe(3)
