MODULE suma_n_liczb
  IMPLICIT NONE
  PUBLIC::Sumuj
CONTAINS
  RECURSIVE FUNCTION Sumuj(i) RESULT (wynik)
  INTEGER, INTENT ( IN)::i
  INTEGER::wynik
  IF (i==0) THEN
  wynik=0
  RETURN
  ELSE
  wynik=i+Sumuj(i-1)
  END IF
  
  WRITE(*, fmt=897) i,wynik
  897 FORMAT ('Suma ', I6,2X,' kolejnych liczb naturalnych =', I10,2X)
  RETURN
  END FUNCTION Sumuj
END MODULE suma_n_liczb

PROGRAM samodzielny_3
  USE suma_n_liczb
  IMPLICIT NONE
  INTEGER::n
  
  PRINT *, 'Wprowadź liczbę naturalną'
  READ (UNIT=*,FMT=*) n
  PRINT *, 'Ostateczny wynik to: ',Sumuj(n)
  STOP
END PROGRAM samodzielny_3