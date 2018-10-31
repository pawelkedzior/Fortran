MODULE zawiera_funkcje_szukaj
  PUBLIC::szukaj
  CONTAINS
    FUNCTION szukaj(lista,klucz) RESULT(szukaj_w)
    CHARACTER (LEN=*), DIMENSION(:), INTENT(IN)::lista
    CHARACTER (LEN=*), INTENT(IN)::klucz
    INTEGER,DIMENSION(2)::szukaj_w
    INTEGER::srodek,ostatni
    
    ostatni=SIZE(lista)
    szukaj_w(1)=0
    szukaj_w(2)=ostatni+1
    
      DO WHILE(szukaj_w(2)-szukaj_w(1)>1)
      srodek=(szukaj_w(1)+szukaj_w(2))/2
      IF (lista(srodek)==klucz) THEN
      szukaj_w=srodek
      EXIT
      ELSE IF (lista(srodek)>klucz) THEN
      szukaj_w(2)=srodek
      ELSE
      szukaj_w(1)=srodek
      END IF
      END DO
    RETURN
    END FUNCTION szukaj
END MODULE zawiera_funkcje_szukaj

PROGRAM prog43
  USE zawiera_funkcje_szukaj
  IMPLICIT NONE
  INTEGER,PARAMETER::rozmiar_tablicy=20, dlugosc_nazwy=20
  CHARACTER (LEN=dlugosc_nazwy),DIMENSION(0:rozmiar_tablicy+1):: tablica_danych
  CHARACTER (LEN=dlugosc_nazwy):: x
  INTEGER Loop,eof
  INTEGER, DIMENSION(2) :: ans
  
  OPEN (UNIT=1, FILE="slowa.txt", STATUS="OLD", ACTION="READ", POSITION="REWIND")
  
    DO LOOP=1, rozmiar_tablicy
    READ (UNIT=1, FMT=*, IOSTAT=eof) tablica_danych(loop)
      IF(eof<0) THEN
      EXIT
      END IF
    END DO
    
  tablica_danych(0)=" "
  tablica_danych(loop)="ZZZZZZZZZZZZZZZZZZZZ"
  WRITE (*,*) "Podaj szukany tekst:"
  READ (UNIT=*,FMT=*) x
  ans=szukaj(tablica_danych(:loop-1),x)
  WRITE (UNIT=*,FMT=*) "Znaleziono za elementem = ", tablica_danych(ans(1)-1)
  WRITE (UNIT=*,FMT=*) "Znaleziono w elemencie = ", tablica_danych(ans(2)-1)
  STOP      
END PROGRAM prog43