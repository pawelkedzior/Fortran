MODULE J11M
  IMPLICIT NONE
  PUBLIC::Calc
CONTAINS
  SUBROUTINE Calc(Z, Answer)
    REAL, INTENT (IN), DIMENSION(:)::Z
    REAL, INTENT (OUT)::Answer
    Answer=sum(Z**2)
    return  
  END SUBROUTINE Calc
END MODULE J11M

PROGRAM prog_47
  USE J11M
  IMPLICIT NONE
  CHARACTER (LEN=79) :: Buffer
  REAL,DIMENSION(13)::Z
  REAL::ANSWER
  INTEGER::N,EoF
  
  OPEN (UNIT=1,FILE="prog47.txt", STATUS="old", ACTION="read", POSITION="rewind")
  N=0
  DO
    N=N+1
    READ (UNIT=1, FMT="(a79)", IOSTAT=EoF) Buffer
    IF (EoF/=0) THEN
      EXIT
    ELSE IF (Buffer(1:1)=="C") THEN
      WRITE (UNIT=*,FMT=*) BUFFER(2:)
    ELSE IF (Buffer(1:1)== " ") THEN
      READ (UNIT=Buffer, FMT="(tr1, 13 f6.3)") Z
      WRITE (UNIT= *,FMT=*) Z
      CALL Calc(Z, Answer)
      WRITE (UNIT=*,FMT=*) Answer
    ELSE
      WRITE (UNIT=*, FMT=*) " Kolumna 1 nie jest pusta lub C w rekordzie numer ",N
    END IF
  END DO
  STOP
END PROGRAM prog_47