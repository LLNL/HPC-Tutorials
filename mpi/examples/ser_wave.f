C ************************************************************************
C  FILE: ser_wave.f
C  DESCRIPTION:
C    Serial Concurrent Wave Equation - Fortran C rsion
C    This program implements the concurrent wave equation described
C    in Chapter 5 of Fox et al., 1988, Solving Problems on Concurrent
C    Processors, vol 1.
C  AUTHOR: unknown. Fortran version by R. Arnowitz.
C  LAST REVISED:  04/15/05 Blaise Barney
C ************************************************************************
C  Explanation of constants and variables used in common blocks and
C  include files
C    tpoints           = total points along wave
C    nsteps            = number of time steps
C    values(0:1001)   = values at time t           HARDCODED LIMIT
C    oldval(0:1001)   = values at time (t-dt)      HARDCODED LIMIT
C    newval(0:1001)   = values at time (t+dt)      HARDCODED LIMIT
C ************************************************************************

      program wave

      implicit none

      print *,'Starting serial version of wave equation...'
      call init_param
      print *,'Initializing points on the line...'
      call init_line
      print *,'Updating all points for all time steps...'
      call update
      print *,'Printing final results...'
      call printfinal
      print *,' '
      print *,'Done.'
      print *,' '
      end

C ************************************************************************
C     Obtains input values from user
C ************************************************************************
      subroutine init_param

      implicit none

C     These are hardcoded limits at the present time do NOT exceed
      integer MAXPOINTS, MAXSTEPS, MINPOINTS
      parameter (MAXPOINTS = 1000)
      parameter (MAXSTEPS = 1000)
      parameter (MINPOINTS = 20)

      integer tpoints, nsteps
      common/inputs/tpoints, nsteps

      tpoints = 0
      nsteps = 0
 
      do while ((tpoints .lt. MINPOINTS) .or. (tpoints .gt. MAXPOINTS))
         write (*,100) MINPOINTS, MAXPOINTS
 100  format(' Enter number of points along vibrating string [',I2,'-',
     &      I5,']:')
         read (*,*) tpoints
         if ((tpoints .lt. MINPOINTS) .or. (tpoints .gt. MAXPOINTS))
     &      print *,'Invalid. Please enter value between ',MINPOINTS,
     &              ' and ',MAXPOINTS
      end do

      do while ((nsteps .lt. 1) .or. (nsteps .gt. MAXSTEPS))
         write (*,101) MAXSTEPS
 101  format(' Enter number of time steps [1-',I4,']:')
         read (*,*) nsteps
         if ((nsteps .lt. 1) .or. (nsteps .gt. MAXSTEPS))
     &      print *,'Invalid. Please enter value between 1 and ', 
     &              MAXSTEPS
      end do

      write (*,102)  tpoints, nsteps
 102  format(' Using points = ', I6, ' steps = ', I5)

      end

C ************************************************************************
C     Initialize points on line
C ************************************************************************
      subroutine init_line

      implicit none
      integer tpoints, nsteps
      common/inputs/tpoints, nsteps
      integer npoints, first
      common/decomp/npoints, first
      real*8 values(0:1001), oldval(0:1001), newval(0:1001)
      common/data/values, oldval, newval
      real*8 PI
      parameter (PI = 3.14159265)
      integer i, j
      real*8 x, fac, k, tmp

C     Calculate initial values based on sine curve
      fac = 2.0 * PI
      k = 0.0
      tmp = tpoints - 1
      do j = 1, tpoints 
         x = k/tmp
         values(j) = sin (fac * x)
         k = k + 1.0
      end do

C     Initialize old values array
      do i = 1, tpoints
         oldval(i) = values(i)
      end do

      end

C ************************************************************************
C     Calculate new values using wave equation
C ************************************************************************
      subroutine do_math(i)

      implicit none
      integer tpoints, nsteps
      common/inputs/tpoints, nsteps
      real*8 values(0:1001), oldval(0:1001), newval(0:1001)
      common/data/values, oldval, newval
      integer i
      real*8 dtime, c, dx, tau, sqtau

      dtime = 0.3
      c = 1.0
      dx = 1.0
      tau = (c * dtime / dx)
      sqtau = tau * tau
      newval(i) = (2.0 * values(i)) - oldval(i) 
     &     + (sqtau * (values(i-1) - (2.0 * values(i)) + values(i+1)))

      end

C ************************************************************************
C     Update all values along line a specified number of times 
C ************************************************************************
      subroutine update

      implicit none
      integer tpoints, nsteps
      common/inputs/tpoints, nsteps
      real*8 values(0:1001), oldval(0:1001), newval(0:1001)
      common/data/values, oldval, newval
      integer i, j;

C     Update values for each time step
      do i = 1, nsteps
C        Update points along line for this time step
         do j = 1, tpoints
C           Global endpoints
            if ((j .eq. 1).or.(j .eq. tpoints))then
               newval(j) = 0.0
	    else
               call do_math(j)
            end if
         end do

C        Update old values with new values
         do j = 1, tpoints
            oldval(j) = values(j)
	    values(j) = newval(j)
         end do
      end do

      end

C ************************************************************************
C     Print final results
C ************************************************************************
      subroutine printfinal
 
      implicit none
      integer tpoints, nsteps
      common/inputs/tpoints, nsteps
      real*8 values(0:1001), oldval(0:1001), newval(0:1001)
      common/data/values, oldval, newval
      integer i, j;

      do i=1, tpoints
         write (*,200) values(i)
         j = MOD(i,10)
         if (j .eq. 0) then
            print *,' '
         endif
      enddo      
 200  format(f6.3,' ',$)
      end
