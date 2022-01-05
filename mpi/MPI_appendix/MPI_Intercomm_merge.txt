MPI_Intercomm_merge(3)                MPI               MPI_Intercomm_merge(3)



NAME
       MPI_Intercomm_merge  -  Creates an intracommuncator from an intercommu-
       nicator

SYNOPSIS
       #include "mpi.h"
       int MPI_Intercomm_merge ( MPI_Comm comm, int high, MPI_Comm *comm_out )

INPUT PARAMETERS
       comm   - Intercommunicator
       high   - Used to order the groups of the two intracommunicators  within
              comm when creating the new communicator.


OUTPUT PARAMETER
       comm_out
              - Created intracommunicator


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


ALGORITHM
       1) Allocate two contexts
       2) Local and remote group leaders swap high values
       3) Determine the high value.
       4) Merge the two groups and make the intra-communicator



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
       MPI_Intercomm_create, MPI_Comm_free

LOCATION
       ic_merge.c



                                  11/14/2001            MPI_Intercomm_merge(3)
