C ****************************************************************************
C  FILE: mpi_pi_reduce.f 
C  DESCRIPTION:  
C    MPI pi Calculation Example - Fortran Version
C    Collective Communications examples
C    This program calculates pi using a "dartboard" algorithm.  See
C    Fox et al.(1988) Solving Problems on Concurrent Processors, vol.1
C    page 207.  All processes contribute to the calculation, with the
C    master averaging the values for pi. SPMD Version:  Conditional 
C    statements check if the process is the master or a worker.
C    This version uses mp_reduce to collect results
C    Note: Requires Fortran90 compiler due to random_number() function
C  AUTHOR: Blaise Barney. Adapted from Ros Leibensperger, Cornell Theory
C    Center. Converted to MPI: George L. Gusciora, MHPCC (1/95)  
C  LAST REVISED: 06/13/13 Blaise Barney
C ****************************************************************************
C Explanation of constants and variables used in this program:
C   DARTS          = number of throws at dartboard 
C   ROUNDS         = number of times "DARTS" is iterated 
C   MASTER         = task ID of master task
C   taskid         = task ID of current task 
C   numtasks       = number of tasks
C   homepi         = value of pi calculated by current task
C   pisum          = sum of tasks' pi values 
C   pi 	           = average of pi for this iteration
C   avepi          = average pi value for all iterations 
C   seednum        = seed number - based on taskid

      program pi_reduce
      include 'mpif.h'

      integer DARTS, ROUNDS, MASTER
      parameter(DARTS = 5000) 
      parameter(ROUNDS = 100)
      parameter(MASTER = 0)

      integer 	taskid, numtasks, i, status(MPI_STATUS_SIZE)
      real*4	seednum
      real*8 	homepi, pi, avepi, pisum, dboard
      external  d_vadd

C     Obtain number of tasks and task ID
      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, taskid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numtasks, ierr )
      write(*,*)'task ID = ', taskid

      avepi = 0

      do 40 i = 1, ROUNDS
C     Calculate pi using dartboard algorithm 
        homepi = dboard(DARTS)

C     Use mp_reduce to sum values of homepi across all tasks
C     Master will store the accumulated value in pisum
C     - homepi is the send buffer
C     - pisum is the receive buffer (used by the receiving task only)
C     - MASTER is the task that will receive the result of the reduction
C       operation
C     - d_vadd is a pre-defined reduction function (double-precision
C       floating-point vector addition)
C     - allgrp is the group of tasks that will participate
        call MPI_REDUCE( homepi, pisum, 1, MPI_DOUBLE_PRECISION, 
     &                   MPI_SUM, MASTER, MPI_COMM_WORLD, ierr )
C     Master computes average for this iteration and all iterations 
        if (taskid .eq. MASTER) then
          pi = pisum/numtasks
          avepi = ((avepi*(i-1)) + pi) / i
          write(*,32) DARTS*i, avepi
 32       format('   After',i6,' throws, average value of pi = ',f10.8) 
        endif
 40   continue

      if (taskid .eq. MASTER) then
        print *, ' '
        print *,'Real value of PI: 3.1415926535897'
        print *, ' '
      endif

      call MPI_FINALIZE(ierr)

      end


C *****************************************************************************
C function dboard.f
C DESCRIPTION:
C   Used in pi calculation example codes.
C   See mpi_pi_send.f and mpi_pi_reduce.f
C   Throw darts at board.  Done by generating random numbers
C   between 0 and 1 and converting them to values for x and y
C   coordinates and then testing to see if they "land" in
C   the circle."  If so, score is incremented.  After throwing the
C   specified number of darts, pi is calculated.  The computed value
C   of pi is returned as the value of this function, dboard.
C   Note: Requires Fortran90 compiler due to random_number() function
C 
C Explanation of constants and variables used in this function:
C   darts    	= number of throws at dartboard
C   score	= number of darts that hit circle
C   n		= index variable
C   r  		= random number between 0 and 1 
C   x_coord	= x coordinate, between -1 and 1  
C   x_sqr	= square of x coordinate
C   y_coord	= y coordinate, between -1 and 1  
C   y_sqr	= square of y coordinate
C   pi		= computed value of pi
C **************************************************************************/

      real*8    function dboard(darts)
      integer   darts, score, n
      real*4	r
      real*8	x_coord, x_sqr, y_coord, y_sqr, pi

      score = 0

C     "throw darts at the board"
      do 10 n = 1, darts
C     generate random numbers for x and y coordinates
        call random_number(r)
        x_coord = (2.0 * r) - 1.0
	x_sqr = x_coord * x_coord

        call random_number(r)
        y_coord = (2.0 * r) - 1.0
	y_sqr = y_coord * y_coord

C       if dart lands in circle, increment score
	if ((x_sqr + y_sqr) .le. 1.0) then
	  score = score + 1
        endif

 10   continue

C     calculate pi
      pi = 4.0 * score / darts
      dboard = pi
      end
