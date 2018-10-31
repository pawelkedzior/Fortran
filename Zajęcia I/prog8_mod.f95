!Kompilacja: f95 prog.f95 (.f95 to rozszerzenie pliku) -o prog
!Wykonywanie: ./prog
!chmod 0700 * (nadaj mi wszystkie przywileje a innym żadne! :D)
PROGRAM osiem
IMPLICIT NONE

TYPE samochod
	CHARACTER(LEN=12)::marka
	CHARACTER(LEN=12)::model
	INTEGER::rocznik
	CHARACTER(LEN=8)::rejestracja
END TYPE samochod

TYPE(samochod)::porsche, FIAT
porsche=samochod("Porsche","Carrera GT",2007,"KR 12345")
FIAT=samochod("FIAT","126p",1985,"KGR BA98")

PRINT *, "Lubię stare samochody."

IF (porsche%rocznik>FIAT%rocznik) THEN
	PRINT *, "Wolę ", FIAT%marka, " ", FIAT%model, " od ", porsche%marka, " ", porsche%model
ELSE
	PRINT *, "Wolę ", porsche%marka, " ", porsche%model, " od ", FIAT%marka, " ", FIAT%model
END IF

STOP
END PROGRAM osiem
