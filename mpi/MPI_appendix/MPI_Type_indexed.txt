MPI_Type_indexed(3)                   MPI                  MPI_Type_indexed(3)



NAME
       MPI_Type_indexed -  Creates an indexed datatype

SYNOPSIS
       #include "mpi.h"
       int MPI_Type_indexed(
               int count,
               int blocklens[],
               int indices[],
               MPI_Datatype old_type,
               MPI_Datatype *newtype )

INPUT PARAMETERS
       count  -  number  of  blocks  --  also number of entries in indices and
              blocklens
       blocklens
              - number of elements in each block (array of  nonnegative  inte-
              gers)
       indices
              -  displacement of each block in multiples of old_type (array of
              integers)
       old_type
              - old datatype (handle)


OUTPUT PARAMETER
       newtype
              - new datatype (handle)


NOTES FOR FORTRAN
       All MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK )  have
       an  additional  argument ierr at the end of the argument list.  ierr is
       an integer and has the same meaning as the return value of the  routine
       in  C.   In Fortran, MPI routines are subroutines, and are invoked with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

       The  indices are displacements, and are based on a zero origin.  A com-
       mon error is to do something like to following
       integer a(100)
       integer blens(10), indices(10)
       do i=1,10
       blens(i)   = 1
       10       indices(i) = 1 + (i-1)*10
       call MPI_TYPE_INDEXED(10,blens,indices,MPI_INTEGER,newtype,ierr)
       call MPI_TYPE_COMMIT(newtype,ierr)
       call MPI_SEND(a,1,newtype,...)

       expecting this to send a(1),a(11),...  because the indices have  values
       1,11,...   .  Because these are displacements from the beginning of a ,
       it actually sends a(1+1),a(1+11),...  .


       If you wish to consider the displacements as  indices  into  a  Fortran
       array, consider declaring the Fortran array with a zero origin
       integer a(0:99)



ERRORS
       All  MPI  routines  (except  MPI_Wtime  and MPI_Wtick ) return an error
       value; C routines as the value of the function and Fortran routines  in
       the last argument.  Before the value is returned, the current MPI error
       handler is called.  By default, this error handler aborts the MPI  job.
       The  error  handler may be changed with MPI_Errhandler_set ; the prede-
       fined error handler MPI_ERRORS_RETURN may be used to cause error values
       to  be  returned.  Note that MPI does not guarentee that an MPI program
       can continue past an error.

       MPI_ERR_COUNT
              - Invalid count argument.  Count arguments must be non-negative;
              a count of zero is often valid.
       MPI_ERR_TYPE
              - Invalid datatype argument.  May be an uncommitted MPI_Datatype
              (see MPI_Type_commit ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.

LOCATION
       type_ind.c



                                  11/14/2001               MPI_Type_indexed(3)
