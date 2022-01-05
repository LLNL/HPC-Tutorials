MPI_Type_struct(3)                    MPI                   MPI_Type_struct(3)



NAME
       MPI_Type_struct -  Creates a struct datatype

SYNOPSIS
       #include "mpi.h"
       int MPI_Type_struct(
               int count,
               int blocklens[],
               MPI_Aint indices[],
               MPI_Datatype old_types[],
               MPI_Datatype *newtype )

INPUT PARAMETERS
       count  - number of blocks (integer) -- also number of entries in arrays
              array_of_types  ,  array_of_displacements   and  array_of_block-
              lengths
       blocklens
              - number of elements in each block (array)
       indices
              - byte displacement of each block (array)
       old_types
              -  type  of elements in each block (array of handles to datatype
              objects)


OUTPUT PARAMETER
       newtype
              - new datatype (handle)


NOTES
       If an upperbound is set explicitly by using the MPI datatype  MPI_UB  ,
       the corresponding index must be positive.

       The  MPI  standard  originally  made vague statements about padding and
       alignment; this was intended to allow the simple definition  of  struc-
       tures that could be sent with a count greater than one.  For example,
       struct { int a; char b; } foo;

       may  have  sizeof(foo)  >  sizeof(int)  +  sizeof(char)  ; for example,
       sizeof(foo) == 2*sizeof(int) .  The initial version of the MPI standard
       defined  the  extent  of  a datatype as including an epsilon that would
       have allowed an implementation to make the extent an MPI  datatype  for
       this structure equal to 2*sizeof(int) .

       However, since different systems might define different paddings, there
       was much discussion by the MPI Forum about what was the  correct  value
       of  epsilon,  and  one  suggestion was to define epsilon as zero.  This
       would have been the best thing to do in MPI 1.0, particularly since the
       MPI_UB  type  allows  the  user to easily set the end of the structure.
       Unfortunately, this change did not make it  into  the  final  document.
       Currently,  this  routine does not add any padding, since the amount of
       padding needed is determined by the compiler that the user is using  to
       build  their  code, not the compiler used to construct the MPI library.
       A later version of MPICH  may  provide  for  some  natural  choices  of
       padding  (e.g.,  multiple of the size of the largest basic member), but
       users are advised to never depend on this, even with vendor MPI  imple-
       mentations.   Instead,  if  you define a structure datatype and wish to
       send or receive multiple items, you should explicitly include an MPI_UB
       entry  as the last member of the structure.  For example, the following
       code can be used for the structure foo
       blen[0] = 1; indices[0] = 0; oldtypes[0] = MPI_INT;
       blen[1] = 1; indices[1] = &foo.b - &foo; oldtypes[1] = MPI_CHAR;
       blen[2] = 1; indices[2] = sizeof(foo); oldtypes[2] = MPI_UB;
       MPI_Type_struct( 3, blen, indices, oldtypes, &newtype );



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
       MPI_ERR_COUNT
              - Invalid count argument.  Count arguments must be non-negative;
              a count of zero is often valid.
       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.

LOCATION
       type_struct.c



                                   7/12/2002                MPI_Type_struct(3)
