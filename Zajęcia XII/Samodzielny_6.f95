PROGRAM samodzielny6
IMPLICIT NONE

INTEGER, PARAMETER :: rdp=SELECTED_REAL_KIND(15)
INTEGER, PARAMETER :: idp=SELECTED_INT_KIND(15)
REAL(KIND=rdp) :: x, y, sinus, moj_wynik, delta
REAL(KIND=rdp) :: pi=3.14159265358979323846264338327950288_rdp
INTEGER(KIND=idp) :: i, wewnatrz
INTEGER, PARAMETER :: maks_razy=1000000_idp

CALL wez_nowe_ziarno()
wewnatrz=0
DO i=1,maks_razy
CALL RANDOM_NUMBER(x)
CALL RANDOM_NUMBER(y)
x=x*pi
sinus=sin(x)
IF(y<=sinus) wewnatrz=wewnatrz+1
END DO
moj_wynik=REAL(wewnatrz)/REAL(maks_razy)*pi
delta=ABS((2-moj_wynik)/2)
PRINT *
PRINT *, "Wynik całki = ", 2, " mój wynik = ", moj_wynik, " błąd względny = ", delta

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
END PROGRAM samodzielny6