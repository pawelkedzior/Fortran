PROGRAM samodzielny4
IMPLICIT NONE

INTEGER :: i,liczba_elementow,n,zamien
REAL:: element
INTEGER, DIMENSION(:), ALLOCATABLE::tablica

  PRINT *, "Podaj liczbę elementów tablicy"
  READ(UNIT=*, FMT=*) liczba_elementow
  ALLOCATE(tablica(liczba_elementow))

  CALL wez_nowe_ziarno()
  
  DO i=1,liczba_elementow
  CALL RANDOM_NUMBER(element)
  tablica(i)=INT(element*10000)
  END DO

  PRINT *,tablica
  
  n=liczba_elementow
  DO WHILE (n>1)
    DO i=1,liczba_elementow-1
      IF (tablica(i)>tablica(i+1)) THEN
	zamien=tablica(i)
	tablica(i)=tablica(i+1)
	tablica(i+1)=zamien
      END IF
    END DO
  n=n-1
  END DO
  
  PRINT *,tablica
  
  DEALLOCATE(tablica)

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
END PROGRAM samodzielny4