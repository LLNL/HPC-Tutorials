C ****************************************************************************
C    Fortran include file for mpl_heat2D.f
C ****************************************************************************
C     implicit none

      integer NXPROB,NYPROB
      parameter(NXPROB = 20)
      parameter(NYPROB = 20)

      real*4 u(NXPROB,NYPROB,2)
      common/grid/u

      real*4  CX,CY
      parameter(CX=0.1)
      parameter(CY=0.1)
