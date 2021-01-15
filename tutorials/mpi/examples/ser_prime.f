C ************************************************************************
C FILE: ser_prime.f
C DESCRIPTION:
C   This program generates primes. The approach taken is a "brute force"
C   method which requires increasingly greater amounts of cpu as the problem
C   size increases. It should lend itself well to an embarrassingly parallel
C   solution since each prime can be computed independently of all other
C   primes.
C AUTHOR: Blaise Barney 11/25/95 - adapted from version contributed by 
C   Richard Ng & Wong Sze Cheong during MHPCC Singapore Workshop (8/22/95).
C LAST REVISED: 04/02/05
C ************************************************************************
C Explanation of constants and variables
C   LIMIT                  = Increase this to find more primes 
C   PRINT                  = Print a line after this many numbers
C   rank                   = task identifier 
C   pc                     = prime counter
C   foundone               = most recent prime found 

      program prime

      integer LIMIT, PRINT
      parameter(LIMIT=2500000)        
      parameter(PRINT=100000)

      integer n, pc, foundone
      logical result

      print *,'Starting. Numbers to be scanned=',LIMIT
C     Assume first four primes are counted here
      pc = 4
      do n=11, LIMIT, 2
         call isprime(n,result)
         if (result .eqv. .true.) then
            pc = pc + 1
            foundone = n
C           ***** Optional: print each prime as it is found
C           print *, foundone
C           *****
         endif
         if (mod(n-1,PRINT).eq.0) then
            print *,'Numbers scanned=',n-1,' Primes found= ',pc
         endif
      enddo
      print *,'Done. Largest prime is ',foundone,' Total primes ',pc
      end


      subroutine isprime (n,result)
      integer n
      logical result
      integer i, squareroot
      real*4 realn
      if (n .gt. 10) then
         realn = real(n)
         squareroot = int (sqrt(realn))
         do i=3,squareroot,2
           if (mod(n,i) .eq. 0) then
              result = .false.
              return
           endif
         enddo
         result = .true.
         return 
C Assume first four primes are counted elsewhere. Forget everything else
      else 
        result = .false.
        return
      endif
      end
