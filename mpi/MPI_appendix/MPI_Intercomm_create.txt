MPI_Intercomm_create(3)               MPI              MPI_Intercomm_create(3)



NAME
       MPI_Intercomm_create  -  Creates an intercommuncator from two intracom-
       municators

SYNOPSIS
       #include "mpi.h"
       int MPI_Intercomm_create ( MPI_Comm local_comm, int local_leader,
                                MPI_Comm peer_comm, int remote_leader, int tag,
                                MPI_Comm *comm_out )

INPUT PARAMTERS
       local_comm
              - Local (intra)communicator
       local_leader
              - Rank in local_comm of leader (often 0)
       peer_comm
              - Remote communicator
       remote_leader
              - Rank in peer_comm of remote leader (often 0)
       tag    - Message tag to use in constructing intercommunicator; if  mul-
              tiple MPI_Intercomm_creates are being made, they should use dif-
              ferent tags (more precisely, ensure that the  local  and  remote
              leaders  are  using different tags for each MPI_intercomm_create
              ).


OUTPUT PARAMETER
       comm_out
              - Created intercommunicator


NOTES
       The MPI 1.1 Standard contains two mutually exclusive  comments  on  the
       input intracommunicators.  One says that their repective groups must be
       disjoint; the other that the leaders can be the  same  process.   After
       some  discussion  by the MPI Forum, it has been decided that the groups
       must be disjoint.  Note that the reason given for this in the  standard
       is  not  the  reason  for  this choice; rather, the other operations on
       intercommunicators (like MPI_Intercomm_merge ) do not make sense if the
       groups are not disjoint.


NOTES FOR FORTRAN
       All  MPI routines in Fortran (except for MPI_WTIME and MPI_WTICK ) have
       an additional argument ierr at the end of the argument list.   ierr  is
       an  integer and has the same meaning as the return value of the routine
       in C.  In Fortran, MPI routines are subroutines, and are  invoked  with
       the call statement.

       All MPI objects (e.g., MPI_Datatype , MPI_Comm ) are of type INTEGER in
       Fortran.


ALGORITHM
       1) Allocate a send context, an inter
              - coll context, and an intra-coll context
       2) Send send_context and lrank_to_grank list from local comm group
              - if I'm the local_leader.
       3) If I'm the local leader, then wait on the posted sends and receives
              - to complete.  Post the receive for the remote  group  informa-
              tion and wait for it to complete.
       4) Broadcast information received from the remote leader.
              - . 5) Create the inter_communicator from the information we now
              have.
       An inter
              - communicator ends up with three levels of communicators.   The
              inter-communicator  returned  to the user, a "collective" inter-
              communicator that can be used for  safe  communications  between
              local  & remote groups, and a collective intra-communicator that
              can be used to allocate new contexts during the  merge  and  dup
              operations.

              For the resulting inter-communicator, comm_out


              comm_out                       = inter-communicator
              comm_out->comm_coll            = "collective" inter-communicator
              comm_out->comm_coll->comm_coll = safe collective intra-communicator



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
       MPI_ERR_COMM
              - Invalid communicator.  A common error is to use a null  commu-
              nicator in a call (not even allowed in MPI_Comm_rank ).
       MPI_ERR_TAG
              -  Invalid  tag  argument.  Tags must be non-negative; tags in a
              receive ( MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.)  may  also
              be MPI_ANY_TAG .  The largest tag value is available through the
              the attribute MPI_TAG_UB .

       MPI_ERR_INTERN
              - This error is returned when some part of the MPICH implementa-
              tion is unable to acquire memory.
       MPI_ERR_RANK
              -  Invalid  source  or  destination rank.  Ranks must be between
              zero and the size of the communicator  minus  one;  ranks  in  a
              receive  (  MPI_Recv , MPI_Irecv , MPI_Sendrecv , etc.) may also
              be MPI_ANY_SOURCE .



SEE ALSO
       MPI_Intercomm_merge, MPI_Comm_free, MPI_Comm_remote_group,
       MPI_Comm_remote_size

LOCATION
       ic_create.c



                                  11/14/2001           MPI_Intercomm_create(3)
