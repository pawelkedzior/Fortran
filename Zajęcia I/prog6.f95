!ósmy progam na tomasz.kozik@uj.edu.pl
 !znaczy ten przerobiony (stworzony własny typ do tego programu

PROGRAM szosty
  IMPLICIT NONE

  CHARACTER(LEN=10) :: data
  CHARACTER(LEN=2) :: miesiac
  
  WRITE(UNIT=*, FMT=*) "Napisz datę w formacie rrrr-mm-dd"
  READ(UNIT=*, FMT=*) data

  STOP
END PROGRAM szosty