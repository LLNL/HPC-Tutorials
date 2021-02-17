C ***************************************************************************
C FILE: ser_heat2D.f
C DESCRIPTIONS:  
C   Serial HEAT2D Example - Fortran Version
C   This example is based on a simplified
C   two-dimensional heat equation domain decomposition.  The initial
C   temperature is computed to be high in the middle of the domain and
C   zero at the boundaries.  The boundaries are held at zero throughout
C   the simulation.  During the time-stepping, an array containing two
C   domains is used; these domains alternate between old data and new data.
C AUTHOR: D. Turner
C LAST REVISED: 04/15/05 Blaise Barney
C *************************************************************************

      program heat2D

      parameter (NXPROB = 100, NYPROB = 100)
      dimension u(NXPROB, NYPROB, 2)
      integer nts, tmp1, tmp2
      real cx, cy
      data cx, cy, nts /0.1, 0.1, 100/

      print *, 'Starting serial version of 2D heat example...'
      tmp1 = NXPROB
      tmp2 = NYPROB
      print *, 'Using (',tmp1,',',tmp2,') grid'
 

c *** Initialize grid and create input file 
      print *, 'Initializing grid and creating input file:'
      call inidat(NXPROB, NYPROB, u(1, 1, 1))
      call prtdat(NXPROB, NYPROB, u(1, 1, 1), 'initial.dat')
      do ix = 1, NXPROB
        u(ix, 1, 2) = u(ix, 1, 1)
        u(ix, NYPROB, 2) = u(ix, NYPROB, 1)
      enddo
      do iy = 1, NYPROB
        u(1, iy, 2) = u(1, iy, 1)
        u(NXPROB, iy, 2) = u(NXPROB, iy, 1)
      enddo

c *** Iterate over all timesteps and create output file
      print *, 'Iterating over',nts,' time steps...'
      iz = 1
      do it = 1, nts
        call update(NXPROB, NYPROB, u(1, 1, iz), u(1, 1, 3-iz),cx,cy)
        iz = 3 - iz
      enddo

      print *, 'Done. Created output file:'
      call prtdat(NXPROB, NYPROB, u(1, 1, iz), 'final.dat')
      stop
      end


c****************************************************************************
c*  subroutine update
c****************************************************************************
      subroutine update(nx, ny, u1, u2, cx, cy)
      dimension u1(nx, ny), u2(nx, ny)
      do ix = 2, nx - 1
        do iy = 2, ny - 1
          u2(ix, iy) = u1(ix, iy)  +
     &                 cx * (u1(ix+1,iy) + u1(ix-1,iy) - 2.*u1(ix,iy)) +
     &                 cy * (u1(ix,iy+1) + u1(ix,iy-1) - 2.*u1(ix,iy))
        enddo
      enddo
c
      return
      end
c
c*****************************************************************************
c*  subroutine inidat
c*****************************************************************************/
      subroutine inidat(nx, ny, u1)
      dimension u1(nx, ny)
      do iy = 1, ny
        do ix = 1, nx
          u1(ix, iy) = float((ix - 1) * (nx - ix) * 
     &                       (iy - 1) * (ny - iy)  )
        enddo
      enddo
c
      return
      end


c**************************************************************************
c* subroutine prtdat
c**************************************************************************
      subroutine prtdat(nx, ny, u1, fnam)
      dimension u1(nx, ny)
      character*(*) fnam
      open(17, file=fnam, status='unknown', err=900)
      do iy = ny, 1, -1
        write(17, 1000) (u1(ix, iy), ix = 1, nx)
1000    format(500(f8.3, 1x))
      enddo
      close(17)
      print *, 'Wrote file: ',fnam
      return
900   continue
      print *, 'Error opening file: ', fnam
      stop
      end
