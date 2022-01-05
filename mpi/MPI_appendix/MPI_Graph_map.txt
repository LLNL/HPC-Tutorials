MPI_Graph_map(3)                      MPI                     MPI_Graph_map(3)



NAME
       MPI_Graph_map -  Maps process to graph topology information

SYNOPSIS
       #include "mpi.h"
       int MPI_Graph_map ( MPI_Comm comm_old, int nnodes, int *index, int *edges,
                          int *newrank )

INPUT PARAMETERS
       comm   - input communicator (handle)
       nnodes - number of graph nodes (integer)
       index  -   integer   array   specifying   the   graph   structure,  see
              MPI_GRAPH_CREATE

       edges  - integer array specifying the graph structure


OUTPUT PARAMETER
       newrank
              - reordered rank of the calling process;  MPI_UNDEFINED  if  the
              calling process does not belong to graph (integer)

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
       MPI_ERR_TOPOLOGY
              - Invalid topology.  Either there is no topology associated with
              this communicator, or it is not the correct type (e.g., MPI_CART
              when expecting MPI_GRAPH ).
       MPI_ERR_COMM
              -  Invalid communicator.  A common error is to use a null commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_ARG
              - Invalid argument.  Some argument is invalid and is not identi-
              fied by a specific error class (e.g., MPI_ERR_RANK ).

LOCATION
       graph_map.c



                                   1/4/2002                   MPI_Graph_map(3)
