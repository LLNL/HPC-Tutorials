MPI_Get_processor_name(3)             MPI            MPI_Get_processor_name(3)



NAME
       MPI_Get_processor_name -  Gets the name of the processor

SYNOPSIS
       #include "mpi.h"
       int MPI_Get_processor_name(
               char *name,
               int *resultlen)

OUTPUT PARAMETERS
       name   -  A  unique  specifier  for  the actual (as opposed to virtual)
              node. This must be an array of  size  at  least  MPI_MAX_PROCES-
              SOR_NAME .

       resultlen
              - Length (in characters) of the name


NOTES
       The  name  returned should identify a particular piece of hardware; the
       exact format is implementation defined.  This name may or  may  not  be
       the same as might be returned by gethostname , uname , or sysinfo .



NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.

LOCATION
       getpname.c



                                  11/14/2001         MPI_Get_processor_name(3)
