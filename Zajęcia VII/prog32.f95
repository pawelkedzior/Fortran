PROGRAM p32
IMPLICIT NONE

REAL :: r(5,5)
CALL wez_nowe_ziarno()
CALL RANDOM_NUMBER(r)
PRINT *, r

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
END PROGRAM p32