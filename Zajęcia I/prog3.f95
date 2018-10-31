!Kompilacja: f95 prog.f95 (.f95 to rozszerzenie pliku) -o prog
!Wykonywanie: ./prog
!chmod 0700 * (nadaj mi wszystkie przywileje a innym żadne! :D)
PROGRAM trzeci
OPEN(UNIT=66, FILE="data_1", STATUS="OLD", ACTION="WRITE", POSITION="APPEND")
WRITE(UNIT=66, FMT=*) "witaj"
STOP
END PROGRAM trzeci