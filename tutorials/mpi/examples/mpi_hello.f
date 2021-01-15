C ****************************************************************************
C FILE: mpi_hello.f
C DESCRIPTION:
C   MPI tutorial example code: Simple hello world program
C AUTHOR: Blaise Barney
C LAST REVISED: 03/05/10
C ****************************************************************************
      program hello
      include 'mpif.h'
      parameter (MASTER = 0)

      integer numtasks, taskid, len, ierr
      character(MPI_MAX_PROCESSOR_NAME) hostname

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numtasks, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierr)

      call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)
      write(*,20) taskid, hostname
      if (taskid .eq. MASTER) then
        write(*,30) numtasks
      end if

      call MPI_FINALIZE(ierr)

  20  format('Hello from task ',I2,' on ',A48)
  30  format('MASTER: Number of MPI tasks is: ',I2)

      end
