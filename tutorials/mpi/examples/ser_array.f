C *****************************************************************************
C FILE: ser_array.f
C DESCRIPTION:
C   Serial Example - Array Assignment - Fortran Version
C   In this simple example, an array is initialized and values assigned.
C AUTHOR: Blaise Barney
C LAST REVISED:  07/15/19
C **************************************************************************

      program array 

      integer   ARRAYSIZE
      parameter (ARRAYSIZE = 20000000)
      integer  index, i, tmp
      real*4   data(ARRAYSIZE)

      print *, 'Starting serial array example...'
      tmp = ARRAYSIZE
      print *, 'Using array of',tmp,' real*4. Requires',tmp*4,' bytes'

C     Initialize the array
      print *, 'Initializing array...'
      do 20 i=1, ARRAYSIZE 
        data(i) =  i * 1.0
 20   continue

      print *, 'Performing computation on array elements...'
      do 50 i=1, ARRAYSIZE
        data(i) = data(i) + i * 1.0
 50   continue

C     Print some results
      print *, 'Sample Results '
      print *, '   data[',1, ']=', data(1) 
      print *, '   data[',100, ']=', data(100) 
      print *, '   data[',1000, ']=', data(1000) 
      print *, '   data[',10000, ']=', data(10000) 
      print *, '   data[',100000, ']=', data(100000) 
      print *, '   data[',1000000, ']=', data(1000000) 
      print *, ' '
      print *, 'All Done!'

      end
