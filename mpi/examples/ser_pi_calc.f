C ************************************************************************
C  FILE: ser_pi_calc.f  
C  DESCRIPTION:  
C    Serial pi Calculation - Fortran Version 
C    This program calculates pi using a "dartboard" algorithm.  See
C    Fox et al.(1988) Solving Problems on Concurrent Processors, vol.1
C    page 207.  
C    Note: Requires Fortran90 compiler due to random_number() function
C  AUTHOR: unknown
C  LAST REVISED: 02/23/12 Blaise Barney 
C ************************************************************************
C Explanation of constants and variables used in this program:
C   DARTS               = number of throws at dartboard
C   ROUNDS              = number of times "DARTS" is iterated
C   pi                  = average of pi for this iteration
C   avepi               = average pi value for all iterations
C ************************************************************************

	program pi_calc

	integer  DARTS, ROUNDS
	parameter(DARTS = 10000)
	parameter(ROUNDS = 100)
        real*4    seednum
        real*8    pi, avepi, dboard

        print *,'Starting serial version of pi calculation...'

 	avepi = 0
	do 40 i = 1, ROUNDS
	  pi = dboard(DARTS)
          avepi = ((avepi*(i-1)) + pi)/ i  
          write(*,32) DARTS*i, avepi
 32       format('   After',i8,' throws, average value of pi = ',
     &           f10.8,$)
 40	continue
        print *, ' '
        print *,'Real value of PI: 3.1415926535897'
        print *, ' '
	end

C ************************************************************************
C  dboard.f
C ************************************************************************
C Explanation of constants and variables used in this function:
C   darts       = number of throws at dartboard
C   score       = number of darts that hit circle
C   n           = index variable
C   r           = random number between 0 and 1
C   x_coord     = x coordinate, between -1 and 1
C   x_sqr       = square of x coordinate
C   y_coord     = y coordinate, between -1 and 1
C   y_sqr       = square of y coordinate
C   pi          = computed value of pi
C ************************************************************************
      real*8    function dboard(darts)

      integer   darts, score, n
      real*4    r
      real*8    x_coord, x_sqr, y_coord, y_sqr, pi

      score = 0

C  Throw darts at board.  Done by generating random numbers
C  between 0 and 1 and converting them to values for x and y
C  coordinates and then testing to see if they "land" in
C  the circle."  If so, score is incremented.  After throwing the
C  specified number of darts, pi is calculated.  The computed value
C  of pi is returned as the value of this function, dboard.
C  Note:  the seed value for rand() is set in pi_calc.

C    Note: Requires Fortran90 compiler due to random_number() function
      print *, ' '
      do 10 n = 1, darts
        call random_number(r)
        x_coord = (2.0 * r) - 1.0
        x_sqr = x_coord * x_coord

        call random_number(r)
        y_coord = (2.0 * r) - 1.0
        y_sqr = y_coord * y_coord

        if ((x_sqr + y_sqr) .le. 1.0) then
          score = score + 1
        endif

 10   continue

      pi = 4.0 * score / darts
      dboard = pi
      end

