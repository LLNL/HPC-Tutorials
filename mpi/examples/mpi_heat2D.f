C ****************************************************************************
C FILE: mpi_heat2D.f
C OTHER FILES: draw_heatf.c mpi_heat2D.h
C DESCRIPTION:  
C   HEAT2D Example - Parallelized Fortran Version
C   This example is based on a simplified two-dimensional heat
C   equation domain decomposition.  The initial temperature is computed to be
C   high in the middle of the domain and zero at the boundaries.  The
C   boundaries are held at zero throughout the simulation.  During the
C   time-stepping, an array containing two domains is used; these domains
C   alternate between old data and new data.
C 
C   In this parallelized version, the grid is decomposed by the master
C   process and then distributed by cols to workers processes.  At each
C   time step, workers processes must exchange border data with neighbors,
C   because a grid point's current temperature depends upon it's previous
C   time step value plus the values of the neighboring grid points.  Upon
C   completion of all time steps, the worker processes return their results
C   to the master process.
C 
C   Two data files are produced: an initial data set and a final data set.
C   An X graphic of these two states displays after all calculations have
C   completed.
C 
C AUTHOR: Blaise Barney - adapted from D. Turner's serial C version. Converted
C   to MPI: George L. Gusciora (1/95)
C LAST REVISED: 06/12/13 Blaise Barney
C ****************************************************************************
C Explanation of constants and variables
C   NXPROB                         =  x dimension of problem grid 
C   NYPROB                         =  y dimension of problem grid
C   STEPS                          =  number of time steps
C   MAXWORKER                      =  maximum number of workers tasks
C   MINWORKER                      =  minimum number of workers tasks
C   BEGIN, LTAG, RTAG, DONE        =  message tags
C   NONE                           =  indicates no neighbor
C   CX, CY                         =  used in heat equation
C   u                              =  array for grids
C   taskid,MASTER                  =  taskids 
C   numworkers                     =  number of workers processes
C   numtasks                       =  number of tasks
C   avecol,cols,offset,extra       =  for sending cols of data
C   dest, source                   =  to - from for message send-receive
C   left,right                     =  neighbor tasks
C   msgtype                        =  for message types
C   rc,start,end                   =  misc
C   i,ix,iy,iz,it                  =  loop variables
C ---------------------------------------------------------------------------

      program heat2D
      include 'mpif.h'
      include 'mpi_heat2D.h'

C     Routine for creating the X graph of the wave
      external draw_heat

      integer STEPS,MAXWORKER,MINWORKER,BEGIN,LTAG,RTAG,DONE,
     .        NONE,MASTER
      parameter(STEPS=50)
      parameter(MAXWORKER=8)
      parameter(MINWORKER=3)
      parameter(BEGIN=1)
      parameter(LTAG=2)
      parameter(RTAG=3)
      parameter(DONE=4)
      parameter(NONE=0)
      parameter(MASTER=0)
      integer taskid,numtasks,numworkers,avecol,cols,offset,extra,
     &        dest,source,left,right,msgtype,
     &        rc,start,end,i,ix,iy,iz,it,ierr
      integer status(MPI_STATUS_SIZE)

C     First, find out my taskid and how many tasks are running */
      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, taskid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numtasks, ierr )
      numworkers = numtasks-1

      if (taskid .eq. MASTER) then
C     ****************************** master code *****************************
C     Check if numworkers is within range - quit if not
      if ((numworkers.lt.MINWORKER).or.(numworkers.gt.MAXWORKER)) then
        print *,'MP_PROCS needs to be between', MINWORKER+1,'and',
     .           MAXWORKER+1, 'for this exercise'
        print *,'Quitting...'
        call MPI_ABORT(MPI_COMM_WORLD, rc, ierr)
      end if

      print *, 'Starting mpi_heat2D with',numworkers,'worker tasks.'

C     Initialize grid 
      print *,'Grid size: X=',NXPROB,' Y=',NYPROB,' Time steps=',STEPS
      print *,'Initializing grid and writing initial.dat file...'
      call inidat
      call prtdat(1)

C     Distribute work to workers.  Must first figure out how many cols to
C     send and what to do with extra cols.  
      avecol=NYPROB/numworkers
      extra=mod(NYPROB,numworkers)
      offset=1
      do i=1, numworkers
        if (i .le. extra) then
          cols=avecol+1
        else
          cols=avecol 
        end if
C       Tell each worker which other workers are its neighbors, since
C       they must exchange data with each other later. 
        if (i .eq. 1) then
          left=NONE
        else  
          left=i-1
        end if
        if (i .eq. numworkers) then
          right=NONE
        else
          right=i+1
        end if 
C       Now send startup information to each worker
        dest = i
      call MPI_SEND( offset, 1, MPI_INTEGER, dest, BEGIN, 
     &               MPI_COMM_WORLD, ierr )
      call MPI_SEND( cols, 1, MPI_INTEGER, dest, BEGIN, 
     &               MPI_COMM_WORLD, ierr )
      call MPI_SEND( left, 1, MPI_INTEGER, dest, BEGIN, 
     &               MPI_COMM_WORLD, ierr )
      call MPI_SEND( right, 1, MPI_INTEGER, dest, BEGIN, 
     &               MPI_COMM_WORLD, ierr )
      call MPI_SEND( u(1,offset,1), cols * NXPROB, MPI_REAL, dest,
     &               BEGIN, MPI_COMM_WORLD, ierr )
        print *,'Sent to=',dest,'offset=',offset,'cols=',cols,
     .    'left=',left,'right=',right
        offset = offset + cols
      end do
          
C     Now wait for results from all workers tasks 
      do i=1, numworkers
        source = i
        msgtype = DONE
        call MPI_RECV( offset, 1, MPI_INTEGER, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
        call MPI_RECV( cols, 1, MPI_INTEGER, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
        call MPI_RECV( u(1,offset,1), cols * NXPROB, MPI_REAL,
     &                 source,msgtype,MPI_COMM_WORLD,status,ierr)
      end do
 
C     Print and show results 
      print *,'Creating final.dat file and generating graph...'
      call prtdat(2)
      call draw_heat()

 
C     End of master code
      call MPI_FINALIZE(ierr)
      end if
 
      if (taskid .ne. MASTER)  then
C     ****************************** worker code ******************************
C     Initialize everything - including the borders - to zero 
      do ix=1,NXPROB
        do iy=1,NYPROB
          do iz=1,2
            u(ix,iy,iz) = 0.0
          end do
        end do
      end do

C     Now receive my offset, cols, neighbors and grid partition from master 
      source = MASTER
      msgtype = BEGIN
      call MPI_RECV( offset, 1, MPI_INTEGER, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
      call MPI_RECV( cols, 1, MPI_INTEGER, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
      call MPI_RECV( left, 1, MPI_INTEGER, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
      call MPI_RECV( right, 1, MPI_INTEGER, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
      call MPI_RECV( u(1,offset,1),cols*NXPROB,MPI_REAL, source,
     &                 msgtype, MPI_COMM_WORLD, status, ierr )
 
C     Determine border elements.  Need to consider first and last columns.
C     Obviously, col 1 can't exchange with col 1-1.  Likewise, the last
C     col can't exchange with last+1. 
      start=offset
      end=offset+cols-1
      if (offset .eq. 1) then
        start=2
      end if
      if (offset + cols .gt. NYPROB) then
        end=end-1
      end if
      print *,'task=',taskid,'  start=',start,'  end=',end 

C     Begin doing STEPS iterations.  Must communicate border cols with
C     neighbors.  If I have the first or last grid col, then I only need to 
C     communicate with one neighbor. 
      iz=1
      do it=1, STEPS
        if (left .ne. NONE) then
          call MPI_SEND( u(1,offset,iz), NXPROB, MPI_REAL, left,
     &                   RTAG, MPI_COMM_WORLD, ierr )

          source = left
          msgtype = LTAG
          call MPI_RECV( u(1,offset-1,iz),NXPROB,MPI_REAL, source,
     &                   msgtype, MPI_COMM_WORLD, status, ierr )
        end if
 
        if (right .ne. NONE) then
          call MPI_SEND(u(1,offset+cols-1,iz),NXPROB,MPI_REAL,
     &                  right,LTAG,MPI_COMM_WORLD,ierr)

          source = right
          msgtype = RTAG
          call MPI_RECV(u(1,offset+cols,iz),NXPROB,MPI_REAL,source,
     &                   msgtype, MPI_COMM_WORLD, status, ierr )
        end if
      
C       Now call update to update the value of grid points
        call update(start,end,u(1,1,iz),u(1,1,3-iz))
        
        iz=3-iz
      end do
    
C     Send my portion of final results back to master 
      call MPI_SEND( offset, 1, MPI_INTEGER, MASTER, DONE, 
     &               MPI_COMM_WORLD, ierr )
      call MPI_SEND( cols, 1, MPI_INTEGER, MASTER, DONE,
     &               MPI_COMM_WORLD, ierr )
      call MPI_SEND( u(1,offset,iz),cols*NXPROB,MPI_REAL,MASTER,
     &               DONE, MPI_COMM_WORLD, ierr )

C     End of worker code
      call MPI_FINALIZE(ierr)
      end if

      end

C****************************************************************************
      subroutine update (start, end, u1, u2)
C****************************************************************************
      include 'mpi_heat2D.h'
      integer start, end, ix, iy
      real*4 u1, u2
      dimension u1(NXPROB,NYPROB),u2(NXPROB,NYPROB)

      do iy=start, end
        do ix=2, NXPROB-1
            u2(ix,iy) = u1(ix,iy)
     .       + CX * ( u1(ix+1,iy) + u1(ix-1,iy) - 2.0 * u1(ix,iy))
     .       + CY * ( u1(ix,iy+1) + u1(ix,iy-1) - 2.0 * u1(ix,iy))
        end do
      end do
	  end

C*****************************************************************************
      subroutine inidat
C*****************************************************************************
      include 'mpi_heat2D.h'
      integer ix,iy

      do ix=0,NXPROB-1
        do iy=0,NYPROB-1
          u(ix+1,iy+1,1) = float(ix*(NXPROB-ix-1) * iy*(NYPROB-iy-1))
        end do
      end do
      end


C**************************************************************************
      subroutine prtdat(i)
C**************************************************************************
      include 'mpi_heat2D.h'
      integer i,ix, iy
      character*11 fname

      if (i .eq. 1) then
        fname ='initial.dat'
      else if (i .eq. 2) then
        fname = 'final.dat'
      end if

      open(21, file=fname, form='formatted')
      do ix=1,NXPROB
        do iy=1,NYPROB
          write(21,'(f8.1,1x,$)')u(ix,iy,1)
        end do
        write(21,'(1x)')
      end do
      close(21)
      end

