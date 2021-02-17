MPI_Buffer_detach(3)                  MPI                 MPI_Buffer_detach(3)



NAME
       MPI_Buffer_detach  -   Removes an existing buffer (for use in MPI_Bsend
       etc)

SYNOPSIS
       #include "mpi.h"
       int MPI_Buffer_detach(
               void *bufferptr,
               int *size )

OUTPUT PARAMETERS
       buffer - initial buffer address (choice)
       size   - buffer size, in bytes (integer)


NOTES
       The reason that MPI_Buffer_detach returns the address and size  of  the
       buffer  being  detached  is  to  allow  nested libraries to replace and
       restore the buffer.  For example, consider

       int size, mysize, idummy;
       void *ptr, *myptr, *dummy;
       MPI_Buffer_detach( &ptr, &size );
       MPI_Buffer_attach( myptr, mysize );
       .
       .
       .
       .
       .
       .
       library code ...
       .
       .
       .
       MPI_Buffer_detach( &dummy, &idummy );
       MPI_Buffer_attach( ptr, size );


       This is much like the action of the Unix signal  routine  and  has  the
       same  strengths (it is simple) and weaknesses (it only works for nested
       usages).

       Note that for this approach  to  work,  MPI_Buffer_detach  must  return
       MPI_SUCCESS  even  when there is no buffer to detach.  In that case, it
       returns a size of zero.  The MPI  1.1  standard  for  MPI_BUFFER_DETACH
       contains the text

       The statements made in this section describe the behavior of MPI for
       buffered-mode sends. When no buffer is currently associated, MPI behaves
       as if a zero-sized buffer is associated with the process.


       This  could  be  read  as  applying only to the various Bsend routines.
       This  implementation  takes  the  position   that   this   applies   to
       MPI_BUFFER_DETACH as well.


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

       The Fortran binding for this routine  is  different.   Because  Fortran
       does  not  have  pointers, it is impossible to provide a way to use the
       output of this routine to exchange buffers.  In  this  case,  only  the
       size field is set.


NOTES FOR C
       Even though the bufferptr argument is declared as void * , it is really
       the address of a void pointer.  See the rationale in the  standard  for
       more details.

LOCATION
       buffree.c



                                  11/14/2001              MPI_Buffer_detach(3)
