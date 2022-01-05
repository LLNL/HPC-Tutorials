MPI_Op_create(3)                      MPI                     MPI_Op_create(3)



NAME
       MPI_Op_create -  Creates a user-defined combination function handle

SYNOPSIS
       #include "mpi.h"
       int MPI_Op_create(
               MPI_User_function *function,
               int commute,
               MPI_Op *op )

INPUT PARAMETERS
       function
              - user defined function (function)
       commute
              - true if commutative;  false otherwise.


OUTPUT PARAMETER
       op     - operation (handle)


NOTES ON THE USER FUNCTION
       The calling list for the user function type is
       typedef void (MPI_User_function) ( void * a,
       void * b, int * len, MPI_Datatype * );

       where  the  operation  is  b[i] = a[i] op b[i] , for i=0,...,len-1 .  A
       pointer to the datatype given to the MPI collective computation routine
       (i.e.,  MPI_Reduce , MPI_Allreduce , MPI_Scan , or MPI_Reduce_scatter )
       is also passed to the user-specified routine.


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


NOTES ON COLLECTIVE OPERATIONS
       The  reduction functions ( MPI_Op ) do not return an error value.  As a
       result, if the functions detect an error, all they  can  do  is  either
       call  MPI_Abort  or silently skip the problem.  Thus, if you change the
       error handler from MPI_ERRORS_ARE_FATAL to something else, for example,
       MPI_ERRORS_RETURN , then no error may be indicated.

       The  reason  for  this is the performance problems in ensuring that all
       collective routines return the same error value.


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
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.


SEE ALSO
       MPI_Op_free

LOCATION
       opcreate.c



                                  11/14/2001                  MPI_Op_create(3)
