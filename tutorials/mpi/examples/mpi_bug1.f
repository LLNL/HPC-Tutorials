C ****************************************************************************
C FILE: mpi_bug1.f
C DESCRIPTION:  
C   This program has a bug that causes it to hang.
C AUTHOR: Blaise Barney 
C LAST REVISED: 04/02/05
C ****************************************************************************

      program mpi_bug1
      include 'mpif.h'

      integer numtasks, rank, dest, source, count, tag, ierr
      integer stat(MPI_STATUS_SIZE)
      character inmsg, outmsg
      outmsg = 'x'

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      print *, 'Task',rank,'starting...' 

      if (rank .eq. 0) then
        if (numtasks .gt. 2) then
          print *, 'Numtasks=',numtasks,'. Only 2 needed.'
          print *, 'Ignoring extra...'
        endif
        dest = rank + 1
        source = dest
        tag = rank
        call MPI_SEND(outmsg, 1, MPI_CHARACTER, dest, tag, 
     &                MPI_COMM_WORLD, ierr)
      print *, 'Sent to task',dest
        call MPI_RECV(inmsg, 1, MPI_CHARACTER, source, tag, 
     &                MPI_COMM_WORLD, stat, ierr)
      print *, 'Received from task',source

      else if (rank .eq. 1) then
        dest = rank - 1
        source = dest
        tag = rank
        call MPI_RECV(inmsg, 1, MPI_CHARACTER, source, tag, 
     &           MPI_COMM_WORLD, stat, err)
      print *, 'Received from task',source
        call MPI_SEND(outmsg, 1, MPI_CHARACTER, dest, tag, 
     &           MPI_COMM_WORLD, err)
      print *, 'Sent to task',dest
      endif

      if (rank .le. 1) then
        call MPI_GET_COUNT(stat, MPI_CHARACTER, count, ierr)
        print *, 'Task ',rank,': Received', count, 'char(s) from task',
     &         stat(MPI_SOURCE), 'with tag',stat(MPI_TAG)
      endif

      call MPI_FINALIZE(ierr)

      end
