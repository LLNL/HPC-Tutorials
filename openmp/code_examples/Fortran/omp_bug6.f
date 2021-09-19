C******************************************************************************
C FILE: omp_bug6.f
C DESCRIPTION:
C   This program compiles and runs fine, but produces the wrong result.
C   Compare to omp_orphan.f.
C AUTHOR: Blaise Barney  6/05
C LAST REVISED: 06/27/05
C******************************************************************************

      PROGRAM ORPHAN
      COMMON /DOTDATA/ A, B
      INTEGER I, VECLEN
      REAL*8 SUM
      PARAMETER (VECLEN = 100)
      REAL*8 A(VECLEN), B(VECLEN)

      DO I=1, VECLEN
         A(I) = 1.0 * I
         B(I) = A(I)
      ENDDO
      SUM = 0.0
!$OMP PARALLEL SHARED (SUM)
      CALL DOTPROD
!$OMP END PARALLEL
      WRITE(*,*) "Sum = ", SUM
      END



      SUBROUTINE DOTPROD
      COMMON /DOTDATA/ A, B
      INTEGER I, TID, OMP_GET_THREAD_NUM, VECLEN
c     REAL*8 SUM
      PARAMETER (VECLEN = 100)
      REAL*8 A(VECLEN), B(VECLEN)

      TID = OMP_GET_THREAD_NUM()
!$OMP DO REDUCTION(+:SUM)
      DO I=1, VECLEN
         SUM = SUM + (A(I)*B(I))
         PRINT *, '  TID= ',TID,'I= ',I
      ENDDO
      RETURN
      END
