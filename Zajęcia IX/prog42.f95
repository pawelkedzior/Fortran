PROGRAM prog42
  IMPLICIT NONE
  REAL::a,b,c
  WRITE (UNIT=*,FMT=*) "PODAJ TRZY LICZBY DO POSORTOWANIA!"
  READ (UNIT=*,FMT=*) a,b,c
  WRITE (UNIT=*,FMT=*) "NO! ALE CO TAK WOLNO?! DAŁEŚ: ", a,b,c
  IF (A>B) CALL SWAP(A,B)
  IF (A>C) CALL SWAP(A,C)
  IF (B>C) CALL SWAP(B,C)
  WRITE (UNIT=*, FMT=*) "POSORTOWAŁEM TAK: ", a,b,c
  STOP
  
  CONTAINS
    SUBROUTINE swap(i1,i2)
      REAL, INTENT (IN OUT) :: i1,i2
      REAL::x
      x=i1
      i1=i2
      i2=x
      RETURN
      END SUBROUTINE swap
      
END PROGRAM prog42