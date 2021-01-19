---
layout: page
title: "Point to Point Communication Routines: Blocking Message Passing Routines"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

The more commonly used MPI blocking message passing routines are described below.

#### [MPI_Send](blocking/MPI_Send.txt)

Basic blocking send operation. Routine returns only after the application buffer in the sending task is free for reuse. Note that this routine may be implemented differently on different systems. The MPI standard permits the use of a system buffer but does not require it. Some implementations may actually use a synchronous send (discussed below) to implement the basic blocking send.
MPI_Send (&buf,count,datatype,dest,tag,comm) 
MPI_SEND (buf,count,datatype,dest,tag,comm,ierr)

#### [MPI_Recv](blocking/MPI_Recv.txt)

Receive a message and block until the requested data is available in the application buffer in the receiving task.
MPI_Recv (&buf,count,datatype,source,tag,comm,&status) 
MPI_RECV (buf,count,datatype,source,tag,comm,status,ierr)

#### [MPI_Ssend](blocking/MPI_Ssend.txt)

Synchronous blocking send: Send a message and block until the application buffer in the sending task is free for reuse and the destination process has started to receive the message.
MPI_Ssend (&buf,count,datatype,dest,tag,comm) 
MPI_SSEND (buf,count,datatype,dest,tag,comm,ierr)

#### [MPI_Sendrecv](blocking/MPI_Sendrecv.txt)

Send a message and post a receive before blocking. Will block until the sending application buffer is free for reuse and until the receiving application buffer contains the received message.

```
MPI_Sendrecv (&sendbuf,sendcount,sendtype,dest,sendtag, 
...... &recvbuf,recvcount,recvtype,source,recvtag, 
...... comm,&status) 
MPI_SENDRECV (sendbuf,sendcount,sendtype,dest,sendtag, 
...... recvbuf,recvcount,recvtype,source,recvtag, 
...... comm,status,ierr)
```

#### [MPI_Wait](blocking/MPI_Wait.txt)
#### [MPI_Waitany](blocking/MPI_Waitany.txt)
#### [MPI_Waitall](blocking/MPI_Waitall.txt)
#### [MPI_Waitsome](blocking/MPI_Waitsome.txt)

MPI_Wait blocks until a specified non-blocking send or receive operation has completed. For multiple non-blocking operations, the programmer can specify any, all or some completions.

```
MPI_Wait (&request,&status) 
MPI_Waitany (count,&array_of_requests,&index,&status) 
MPI_Waitall (count,&array_of_requests,&array_of_statuses) 
MPI_Waitsome (incount,&array_of_requests,&outcount, 
...... &array_of_offsets, &array_of_statuses) 
MPI_WAIT (request,status,ierr) 
MPI_WAITANY (count,array_of_requests,index,status,ierr) 
MPI_WAITALL (count,array_of_requests,array_of_statuses, 
...... ierr) 
MPI_WAITSOME (incount,array_of_requests,outcount, 
...... array_of_offsets, array_of_statuses,ierr)
```

#### [MPI_Probe](blocking/MPI_Probe.txt)

Performs a blocking test for a message. The "wildcards" MPI_ANY_SOURCE and MPI_ANY_TAG may be used to test for a message from any source or with any tag. For the C routine, the actual source and tag will be returned in the status structure as status.MPI_SOURCE and status.MPI_TAG. For the Fortran routine, they will be returned in the integer array status(MPI_SOURCE) and status(MPI_TAG).  

```
MPI_Probe (source,tag,comm,&status) 
MPI_PROBE (source,tag,comm,status,ierr)
```

#### [MPI_Get_count](blocking/MPI_Get_count.txt)

Returns the source, tag and number of elements of datatype received. Can be used with both blocking and non-blocking receive operations. For the C routine, the actual source and tag will be returned in the status structure as status.MPI_SOURCE and status.MPI_TAG. For the Fortran routine, they will be returned in the integer array status(MPI_SOURCE) and status(MPI_TAG).

```
MPI_Get_count (&status,datatype,&count) 
MPI_GET_COUNT (status,datatype,count,ierr)
```
