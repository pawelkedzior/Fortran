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

PROGRAM samodzielny_1
  IMPLICIT NONE
  REAL:: a,b,c,delta,x1,x2
  
  PRINT *, 'Podaj współczynnik a równania kwadratowego (ax^2+bx+c=0)'
  READ (UNIT=*,FMT=*) a
  PRINT *, 'Podaj współczynnik b'
  READ (UNIT=*,FMT=*) b
  PRINT *, 'Podaj współczynnik c'
  READ (UNIT=*,FMT=*) c
  WRITE (*, fmt=789) a,b,c
  789 FORMAT ('Twoje równanie wygląda następująco: ', F6.3 ,'*x^2+', F6.3,'*x+', F6.3,'=0')
  delta=b*b-(4*a*c)
  WRITE (*,*) 'Delta jest równa=',delta
  IF (delta<0) THEN
    PRINT *,'Delta jest mniejsza od 0 - brak miejsc zerowych'
  ELSE IF (delta==0) THEN
    PRINT *,'Delta równa 0 - jedno miejsce zerowe, równe ',((-1)*b/(2*a))
  ELSE
    x1=((-1)*b+sqrt(delta))/(2*a)
    x2=((-1)*b-sqrt(delta))/(2*a)
    PRINT *,'Delta większa od 0 - dwa miejsca zerowe - jedno równe ',x1,' drugie równe ',x2
  END IF
  STOP
END PROGRAM samodzielny_1