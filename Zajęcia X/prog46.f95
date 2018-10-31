MODULE zawiera_funkcje_rekursywna
  IMPLICIT NONE
  PUBLIC::Euclid
CONTAINS
  RECURSIVE FUNCTION Euclid(i1, i2) RESULT (gcd)
  INTEGER, INTENT ( IN)::i1,i2
  INTEGER::gcd
  INTEGER::remainder
  remainder=MOD(i1,i2)
  IF (remainder==0) THEN
  gcd=i2
  RETURN
  ELSE
  gcd=EUCLID(i2,remainder)
  END IF
  WRITE(*, fmt=897) i1,i2,remainder,gcd
  897 FORMAT ('Reszta z dzielenia liczby =', I6,2X,'przez liczbę =', I4,2X,'daje resztę =' I4,2X, 'WYNIK =', I4)
  RETURN
  END FUNCTION Euclid
END MODULE zawiera_funkcje_rekursywna

PROGRAM prog_46
  USE zawiera_funkcje_rekursywna
  IMPLICIT NONE
  INTEGER::p,q
  
  PRINT *, 'Wprowadź liczbę naturalną do dzielenia "p"'
  READ (UNIT=*,FMT=*) p
  PRINT *, 'Wprowadź dzielnik "q"'
  READ (UNIT=*,FMT=*) q
  PRINT *, Euclid(p,q), 'to największy wspólny dzielnik zwracany przez Euclid(p,q)'
  STOP
END PROGRAM prog_46