MPI_Comm_group(3)                     MPI                    MPI_Comm_group(3)



NAME
       MPI_Comm_group -  Accesses the group associated with given communicator

SYNOPSIS
       #include "mpi.h"
       int MPI_Comm_group (
               MPI_Comm comm,
               MPI_Group *group )

INPUT PARAMETER
       comm   - Communicator


OUTPUT PARAMETER
       group  - Group in communicator


USING 'MPI_COMM_NULL' WITH 'MPI_COMM_GROUP'
       It is an error  to  use  MPI_COMM_NULL  as  one  of  the  arguments  to
       MPI_Comm_group .  The relevant sections of the MPI standard are

       .   (2.4.1  Opaque  Objects)  A null handle argument is an erroneous IN
       argument in MPI calls, unless an exception is explicitly stated in  the
       text that defines the function.

       .   (5.3.2.  Group  Constructors) <no text in MPI_COMM_GROUP allowing a
       null handle>

       Previous versions of MPICH allow MPI_COMM_NULL in  this  function.   In
       the interests of promoting portability of applications, we have changed
       the behavior of MPI_Comm_group to detect  this  violation  of  the  MPI
       standard.


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

LOCATION
       comm_group.c



                                  11/14/2001                 MPI_Comm_group(3)
