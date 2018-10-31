MODULE gra
IMPLICIT NONE
  PUBLIC::rysuj_plansze
TYPE polozenie
  integer:: x
  integer:: y
END TYPE polozenie

TYPE stan
  CHARACTER,dimension(0:2,0:2)::plansza
END TYPE

Contains
  SUBROUTINE rysuj_plansze(obecny_stan)
    TYPE(stan):: obecny_stan
    CHARACTER(LEN=5),DIMENSION(1:5)::plansza
    INTEGER:: i,j
    DO i=0,2
	plansza(i+1)=obecny_stan%plansza(0,i)//"|"//obecny_stan%plansza(1,i)//"|"//obecny_stan%plansza(2,i)
    END DO
    plansza(5)=plansza(3)
    plansza(3)=plansza(2)
    plansza(2)="-|-|-"
    plansza(4)="-|-|-"

    DO i=1,5
      PRINT *,plansza(i)
    END DO
  END SUBROUTINE rysuj_plansze

  FUNCTION wykonajRuch(kto) RESULT(ruch)
    LOGICAL,INTENT(IN)::kto
    TYPE(polozenie)::ruch
    INTEGER::x,y
    IF (kto) THEN
      PRINT *,"Kolej na kółka. Gdzie wykonujesz ruch? Najpierw współrzędna x"
      READ *,x
      PRINT *,"Teraz y"
      READ *,y
      ruch%x=x
      ruch%y=y
      RETURN
    ELSE
      PRINT *,"Kolej na krzyzyki. Gdzie wykonujesz ruch? Najpierw współrzędna x"
      READ *,x
      PRINT *,"Teraz y"
      READ *,y
      ruch%x=x
      ruch%y=y
      RETURN  
    END IF
  END FUNCTION wykonajRuch
  
  FUNCTION czyKoniec(obecny_stan) RESULT(czy_koniec)
    TYPE(stan)::obecny_stan
    INTEGER::czy_koniec
    INTEGER::ile
    INTEGER::i,j
    
    ile=0
    czy_koniec=0
    DO j=0,2
      DO i=0,2
		IF (obecny_stan%plansza(j,i).ne." ") THEN
			ile=ile+1
		END IF
		IF ((obecny_stan%plansza(0,i).eq.obecny_stan%plansza(1,i)).and.(obecny_stan%plansza(1,i).eq.obecny_stan%plansza(2,i))) THEN
		  	IF (obecny_stan%plansza(0,i).eq."X") THEN
				czy_koniec=2
		  		RETURN
			ELSE IF (obecny_stan%plansza(0,i).eq."O") THEN
				czy_koniec=1
		  		RETURN
			END IF
		END IF
		IF	((obecny_stan%plansza(i,0).eq.obecny_stan%plansza(i,1)).and.(obecny_stan%plansza(i,1).eq.obecny_stan%plansza(i,2))) THEN
		  	IF (obecny_stan%plansza(i,0).eq."X") THEN
				czy_koniec=2
		  		RETURN
			ELSE IF (obecny_stan%plansza(i,0).eq."O") THEN
				czy_koniec=1
		  		RETURN
			END IF
		END IF
		IF (((obecny_stan%plansza(0,0).eq.obecny_stan%plansza(1,1)).and.(obecny_stan%plansza(1,1).eq.obecny_stan%plansza(2,2))).or.&
	  		((obecny_stan%plansza(0,2).eq.obecny_stan%plansza(1,1)).and.(obecny_stan%plansza(1,1).eq.obecny_stan%plansza(2,0)))) THEN	
	  		IF (obecny_stan%plansza(1,1).eq."X") THEN
	    		czy_koniec=2
		  	ELSE IF (obecny_stan%plansza(1,1).eq."O") THEN
	    		czy_koniec=1
	  		END IF
	  		RETURN
		END IF	
      END DO
      IF (ile.eq.9) THEN
		czy_koniec=3
		RETURN
      END IF	
    END DO
    RETURN
  END FUNCTION czyKoniec 
END MoDULE gra

PROGRAM kolko_krzyzyk
USE gra
IMPLICIT NONE

TYPE(polozenie)::ostatni_ruch
TYPE(stan)::stan_gry
LOGICAL::kolka
INTEGER::wynik
  
  wynik=0
  kolka=.TRUE.
  stan_gry%plansza=" "
  DO
    ostatni_ruch=wykonajRuch(kolka)
    if (kolka) then
      stan_gry%plansza(ostatni_ruch%x,ostatni_ruch%y)="O"
    else
      stan_gry%plansza(ostatni_ruch%x,ostatni_ruch%y)="X"
    end if
    CALL rysuj_plansze(stan_gry)
	wynik=czyKoniec(stan_gry)
    IF (wynik==1) THEN
      PRINT *,"KONIEC! Wygrały kółka!"
      EXIT
    ELSE IF (wynik==2) THEN
      PRINT *,"KONIEC! Wygrały krzyżyki!"
      EXIT
    ELSE IF (wynik==3) THEN
      PRINT *,"KONIEC! Nikt nie wygrał."
      EXIT
    ELSE
      PRINT *,"Kolejny ruch."
    END IF
    kolka=.not.kolka
  END DO
  STOP
END PROGRAM kolko_krzyzyk
