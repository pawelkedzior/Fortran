MODULE zawiera_funkcje_licz_sam
  PUBLIC::licz_sam
  CONTAINS
    FUNCTION ;licz_sam(lista) RESULT(liczba)
    CHARACTER (LEN=*), INTENT(IN)::slowo
    INTEGER::liczba,i,dlug_slowa
    liczba=0

    DO i=0,dlug_slowa
    
      SELECT CASE(slowo(i))
      
	CASE ("a")
	
	CASE ("e")
	
	CASE ("i")
	
	CASE ("y")
	
	CASE ("o")
	
	CASE ("u")
	
	CASE ("ą")
	
	CASE ("ę")
     
      END SELECT
    
    END DO
      
    RETURN
    END FUNCTION szukaj
END MODULE zawiera_funkcje_licz_sam

PROGRAM samodzileny5
  USE zawiera_funkcje_licz_sam
  IMPLICIT NONE
  INTEGER,PARAMETER::rozmiar_tablicy=20, dlugosc=60
  CHARACTER (LEN=dlugosc):: slowo
  INTEGER Loop,eof
  INTEGER, DIMENSION(2) :: ans

  STOP      
END PROGRAM samodzielny5