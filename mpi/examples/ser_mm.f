C******************************************************************************
C FILE: ser_mm.f
C DESCRIPTION: 
C   Serial Matrix Multiply - Fortran version.
C AUTHOR: unknown
C LAST REVISED:  04/15/05 Blaise Barney 
C******************************************************************************

      program mm

      parameter (NRA = 62)
      parameter (NCA = 15)
      parameter (NCB = 7)

      integer 	i, j, k
      real*8	a(NRA,NCA), b(NCA,NCB), c(NRA,NCB)

      print *,'Starting serial matrix multiple example...'
      write(*,100) NRA,NCA,NCA,NCB,NRA,NCB
 100  format(' Using matrix sizes a(',I2,',',I2,'),b(',I2,',',I1,
     & '),c(',I2,',',I1,')')

C     Initialize A and B 
      print *,'Initializing matrices...'
      do 30 i=1, NRA
        do 30 j=1, NCA
        a(i,j) = (i-1)+(j-1)
 30   continue
      do 40 i=1, NCA
        do 40 j=1, NCB
	b(i,j) = (i-1)*(j-1)
 40   continue

C     Do matrix multiply
      print *,'Performing matrix multiply...'
       do 50 k=1, NCB 
         do 50 i=1, NRA
         c(i,k) = 0.0
           do 50 j=1, NCA
           c(i,k) = c(i,k) + a(i,j) * b(j,k)
  50       continue

C     Print results 
      print*, 'Here is the result matrix: '	
      do 90 i=1, NRA
         do 80 j = 1, NCB
         write(*,70)c(i,j)
  70     format(2x,f8.2,$)
  80     continue
      print *, ' '
  90  continue
      print *, 'Done.'
      print *, ' '

      end
