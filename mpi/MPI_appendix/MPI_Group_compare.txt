MPI_Group_compare(3)                  MPI                 MPI_Group_compare(3)



NAME
       MPI_Group_compare -  Compares two groups

SYNOPSIS
       #include "mpi.h"
       int MPI_Group_compare ( MPI_Group group1, MPI_Group group2, int *result )

INPUT PARAMETERS
       group1 - group1 (handle)
       group2 - group2 (handle)


OUTPUT PARAMETER
       result - integer which is MPI_IDENT if the order and members of the two
              groups are the same, MPI_SIMILAR if only  the  members  are  the
              same, and MPI_UNEQUAL otherwise


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

LOCATION
       groupcompare.c



                                  11/14/2001              MPI_Group_compare(3)
