PROGRAM p12
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: range=SELECTED_INT_KIND(20)
  INTEGER(KIND = range) :: suma, n
  PRINT *, "range = ", range
  suma=0_range
  n=0_range
  
  DO
    n=n+1
    IF (n>1234567890) EXIT
    IF (n==55) CYCLE !przejście do następnego kroku pętli bez wykonywania dalszych części
    IF (n.EQ.122) CYCLE ! .EQ. to to samo co ==
    IF (n>=20.AND.n.LE.30) CYCLE !.LE. to to samo co <=
    IF (n.GE.20.AND.n<=30) CYCLE !.GE. -||- >=
    suma=suma+n
  END DO
  
  PRINT *, "suma: ", suma
  
END PROGRAM p12