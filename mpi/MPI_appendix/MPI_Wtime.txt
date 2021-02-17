MPI_Wtime(3)                          MPI                         MPI_Wtime(3)



NAME
       MPI_Wtime -  Returns an elapsed time on the calling processor

SYNOPSIS
       #include "mpi.h"
       double MPI_Wtime()

RETURN VALUE
       Time in seconds since an arbitrary time in the past.


NOTES
       This is intended to be a high-resolution, elapsed (or wall) clock.  See
       MPI_WTICK to determine the resolution of MPI_WTIME .

       If the attribute MPI_WTIME_IS_GLOBAL is  defined  and  true,  then  the
       value is synchronized across all processes in MPI_COMM_WORLD .



NOTES FOR FORTRAN
       This  is  a  function, declared as DOUBLE PRECISION MPI_WTIME() in For-
       tran.


SEE ALSO
       also: MPI_Wtick, MPI_Attr_get

LOCATION
       wtime.c



                                  11/14/2001                      MPI_Wtime(3)
