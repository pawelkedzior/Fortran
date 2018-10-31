PROGRAM p31
IMPLICIT NONE

integer, parameter :: rdp=SELECTED_REAL_KIND(15)
REAL(KIND=RDP) :: x, xx
integer :: i

DO i=1,5
CALL RANDOM_NUMBER(x)
CALL RANDOM_NUMBER(xx)
PRINT *, x, xx
END DO

CALL wez_nowe_ziarno()
PRINT *

DO i=1,5
CALL RANDOM_NUMBER(x)
CALL RANDOM_NUMBER(xx)
PRINT *, x, xx
END DO

STOP
CONTAINS
  SUBROUTINE wez_nowe_ziarno()
    INTEGER :: i, n, zegar
    INTEGER, DIMENSION(:), ALLOCATABLE :: ziarno
    
    CALL RANDOM_SEED(SIZE=n)
    PRINT *, "SIZE = ", n
    ALLOCATE(ziarno(n))
    CALL RANDOM_SEED(GET=ziarno)
    PRINT *, "ZIARNO = ", ziarno
    CALL SYSTEM_CLOCK(COUNT=zegar)
    PRINT *, "ZEGAR = ", zegar
    ziarno=zegar+37*(/ (i-1, i=1,n) /)
    CALL RANDOM_SEED(PUT = ziarno)
    PRINT *, "ZIARNO = ", ziarno
    
    DEALLOCATE(ziarno)
  END SUBROUTINE
END PROGRAM p31