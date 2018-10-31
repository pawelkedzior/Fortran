!Kompilacja: f95 prog.f95 (.f95 to rozszerzenie pliku) -o prog
!Wykonywanie: ./prog
!chmod 0700 * (nadaj mi wszystkie przywileje a innym żadne! :D)
PROGRAM osiem
IMPLICIT NONE

TYPE osoba
	CHARACTER(LEN=12)::pierwsze_imie
	CHARACTER(LEN=1)::inicjal_drugiego
	CHARACTER(LEN=12)::nazwisko
	INTEGER::wiek
	CHARACTER(LEN=1)::plec
	CHARACTER(LEN=11)::ubezpieczenie_spoleczne
END TYPE osoba

TYPE(osoba)::jacek, juliana
jacek=osoba("Jacek","R","Haski",47,"M","123-45-6789")
juliana=osoba("Juliana","M","Kowalska",39,"K","987-65-4321")

PRINT *,juliana%nazwisko

PRINT *, jacek%wiek

IF (jacek%plec=="K") PRINT *, jacek%pierwsze_imie, " jest kobietą"
IF (juliana%plec=="K") PRINT *, juliana%pierwsze_imie, " jest kobietą"

PRINT *,jacek

STOP
END PROGRAM osiem
