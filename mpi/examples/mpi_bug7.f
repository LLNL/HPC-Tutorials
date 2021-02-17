C******************************************************************************
C FILE: mpi_bug7.f
C DESCRIPTION:
C   This program hangs. 
C AUTHOER: Blaise Barney
C LAST REVISED: 01/24/09
C*****************************************************************************/

      program bug7
      include 'mpif.h'

      integer  numtasks, taskid, len, buffer, root, count, ierr
      character(MPI_MAX_PROCESSOR_NAME) hostname

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierr)

      call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)
      write(*,20) taskid, hostname

      buffer = 23
      root = 0
      count = taskid

      if (taskid .eq. root) then
        write(*,30) numtasks
      end if

      call MPI_BCAST(buffer, count, MPI_INTEGER, root, MPI_COMM_WORLD, 
     &               ierr)

      call MPI_FINALIZE(ierr)

  20  format('Task ',I2,' on ',A10,' starting...')
  30  format('Root: Number of MPI tasks is: ',I2)

      end

