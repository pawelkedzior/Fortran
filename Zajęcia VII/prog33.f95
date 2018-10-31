PROGRAM p33
IMPLICIT NONE

INTEGER, PARAMETER :: rdp=SELECTED_REAL_KIND(15)
INTEGER, PARAMETER :: idp=SELECTED_INT_KIND(15)
REAL(KIND=rdp) :: xx, yy, rr, pi_moje, delta
REAL(KIND=rdp) :: pi=3.14159265358979323846264338327950288_rdp
REAL :: x
INTEGER(KIND=idp) :: i, wewnatrz
INTEGER, PARAMETER :: maks_razy=1000000_idp

CALL wez_nowe_ziarno()
wewnatrz=0
DO i=1,maks_razy
CALL RANDOM_NUMBER(xx)
CALL RANDOM_NUMBER(yy)
rr=xx*xx+yy*yy
IF(rr<=1.0_rdp) wewnatrz=wewnatrz+1
END DO
pi_moje=REAL(wewnatrz)/REAL(maks_razy)*4.0_rdp
delta=ABS((pi-pi_moje)/pi)
PRINT *
PRINT *, "pi porównywane = ", pi, " moje pi = ", pi_moje, " błąd względny = ", delta

STOP
CONTAINS
  SUBROUTINE wez_nowe_ziarno()
    INTEGER :: i, n, zegar
    INTEGER, DIMENSION(:), ALLOCATABLE :: ziarno
    
    CALL RANDOM_SEED(SIZE=n)
    PRINT *, "SIZE = ", n
    ALLOCATE(ziarno(n))
    
    CALL SYSTEM_CLOCK(COUNT=zegar)
    PRINT *, "ZEGAR = ", zegar
    
    ziarno=zegar+37*(/ (i-1, i=1,n) /)
    CALL RANDOM_SEED(PUT = ziarno)
    
    DEALLOCATE(ziarno)
  END SUBROUTINE
END PROGRAM p33