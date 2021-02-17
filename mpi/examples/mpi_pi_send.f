C ****************************************************************************
C  FILE: mpi_pi_send.f
C  DESCRIPTION:  
C    MPI pi Calculation Example - Fortran Version 
C    Point-to-Point Communication example
C    This program calculates pi using a "dartboard" algorithm.  See
C    Fox et al.(1988) Solving Problems on Concurrent Processors, vol.1
C    page 207.  All processes contribute to the calculation, with the
C    master averaging the values for pi. SPMD Version:  Conditional 
C    statements check if the process is the master or a worker.
C    This version uses low level sends and receives to collect results 
C    Note: Requires Fortran90 compiler due to random_number() function
C  AUTHOR: Blaise Barney. Adapted from Ros Leibensperger, Cornell Theory
C   Center. Converted to MPI: George L. Gusciora, MHPCC (1/95)  
C  LAST REVISED: 06/13/13 Blaise Barney
C ****************************************************************************
C Explanation of constants and variables used in this program:
C   DARTS          = number of throws at dartboard 
C   ROUNDS         = number of times "DARTS" is iterated 
C   MASTER         = task ID of master task
C   taskid         = task ID of current task 
C   numtasks       = number of tasks
C   homepi         = value of pi calculated by current task
C   pi             = average of pi for this iteration
C   avepi          = average pi value for all iterations 
C   pirecv         = pi received from worker 
C   pisum          = sum of workers' pi values 
C   seednum        = seed number - based on taskid
C   source         = source of incoming message
C   mtype          = message type 
C   i, n           = misc

      program pi_send 
      include 'mpif.h'

      integer DARTS, ROUNDS, MASTER
      parameter(DARTS = 5000) 
      parameter(ROUNDS = 100)
      parameter(MASTER = 0)

      integer 	taskid, numtasks, source, mtype, i, n,
     &          status(MPI_STATUS_SIZE)
      real*4	seednum
      real*8 	homepi, pi, avepi, pirecv, pisum, dboard

C     Obtain number of tasks and task ID
      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, taskid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numtasks, ierr )
      write(*,*)'task ID = ', taskid

      avepi = 0

      do 40 i = 1, ROUNDS
C     Calculate pi using dartboard algorithm 
      homepi = dboard(DARTS)

C     ******************** start of worker section ***************************
C     All workers send result to master.  Steps include:  
C     -set message type equal to this round number
C     -set message size to 8 bytes (size of real8)
C     -send local value of pi (homepi) to master task
      if (taskid .ne. MASTER) then
        mtype = i 
        sbytes = 8	
        call MPI_SEND( homepi, 1, MPI_DOUBLE_PRECISION, MASTER, i, 
     &                 MPI_COMM_WORLD, ierr )

C     ******************** end of worker section *****************************
      else
C     ******************** start of master section **************************
C     Master receives messages from all workers.  Steps include:
C     -set message type equal to this round 
C     -set message size to 8 bytes (size of real8)
C     -receive any message of type mytpe
C     -keep running total of pi in pisum
C     Master then calculates the average value of pi for this iteration 
C     Master also calculates and prints the average value of pi over all 
C     iterations 
        mtype = i	
        sbytes = 8
        pisum = 0
        do 30 n = 1, numtasks-1
          call MPI_RECV( pirecv, 1, MPI_DOUBLE_PRECISION,
     &       MPI_ANY_SOURCE, mtype, MPI_COMM_WORLD, status, ierr )
          pisum = pisum + pirecv
 30     continue
        pi = (pisum + homepi)/numtasks
        avepi = ((avepi*(i-1)) + pi) / i
        write(*,32) DARTS*i, avepi
 32     format('   After',i6,' throws, average value of pi = ',f10.8) 
C    ********************* end of master section ****************************
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
