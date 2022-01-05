MPI_Group_incl(3)                     MPI                    MPI_Group_incl(3)



NAME
       MPI_Group_incl  -  Produces a group by reordering an existing group and
       taking only listed members

SYNOPSIS
       #include "mpi.h"
       int MPI_Group_incl ( MPI_Group group, int n, int *ranks, MPI_Group *group_out )

INPUT PARAMETERS
       group  - group (handle)
       n      - number of elements in array ranks  (and  size  of  newgroup  )
              (integer)
       ranks  -  ranks  of  processes in group to appear in newgroup (array of
              integers)


OUTPUT PARAMETER
       newgroup
              - new group derived from above, in the order defined by ranks

              (handle)


NOTE
       This implementation does not currently check to see that  the  list  of
       ranks to ensure that there are no duplicates.


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
       MPI_ERR_GROUP
              - Null group passed to function.
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.
       MPI_ERR_RANK
              -  Invalid  source  or  destination rank.  Ranks must be between
              zero and the size of the communicator  minus  one;  ranks  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_SOURCE .



SEE ALSO
       MPI_Group_free

LOCATION
       group_incl.c



                                  11/14/2001                 MPI_Group_incl(3)
