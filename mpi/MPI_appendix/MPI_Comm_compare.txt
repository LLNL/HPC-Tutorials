MPI_Comm_compare(3)                   MPI                  MPI_Comm_compare(3)



NAME
       MPI_Comm_compare -  Compares two communicators

SYNOPSIS
       #include "mpi.h"
       int MPI_Comm_compare (
               MPI_Comm  comm1,
               MPI_Comm  comm2,
               int *result)

INPUT PARAMETERS
       comm1  - comm1 (handle)
       comm2  - comm2 (handle)


OUTPUT PARAMETER
       result -  integer which is MPI_IDENT if the contexts and groups are the
              same, MPI_CONGRUENT if different contexts but identical  groups,
              MPI_SIMILAR  if  different  contexts  but  similar  groups,  and
              MPI_UNEQUAL otherwise


USING 'MPI_COMM_NULL' WITH 'MPI_COMM_COMPARE'
       It is an error  to  use  MPI_COMM_NULL  as  one  of  the  arguments  to
       MPI_Comm_compare .  The relevant sections of the MPI standard are

       .   (2.4.1  Opaque  Objects)  A null handle argument is an erroneous IN
       argument in MPI calls, unless an exception is explicitly stated in  the
       text that defines the function.

       .   (5.4.1. Communicator Accessors) <no text in MPI_COMM_COMPARE allow-
       ing a null handle>


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
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       commcompare.c



                                  11/14/2001               MPI_Comm_compare(3)
