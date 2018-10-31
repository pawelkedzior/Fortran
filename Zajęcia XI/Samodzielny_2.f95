MODULE wyznacznik
  IMPLICIT NONE
  PUBLIC:: licz_wyznacznik
  CONTAINS
    FUNCTION licz_wyznacznik(a1,b1,a2,b2) RESULT(licz_wyznacznik_w)
    REAL, INTENT(IN)::a1,b1,a2,b2
    REAL:: licz_wyznacznik_w
    licz_wyznacznik_w=(a1*b2)-(b1*a2)
    RETURN
    END FUNCTION licz_wyznacznik
END MODULE wyznacznik

Program samodzielny_2
  USE wyznacznik
  IMPLICIT NONE
  
  TYPE rownanie
    REAL::a,b,c
  END TYPE rownanie
  
  REAL::d,e,f,Wx,Wy,W,x,y
  TYPE(rownanie)::pierwsze, drugie
  PRINT *, 'Podaj współczynnik a pierwszego równania (ax+by=c)'
  READ (UNIT=*,FMT=*) d
  PRINT *, 'Podaj współczynnik b'
  READ (UNIT=*,FMT=*) e
  PRINT *, 'Podaj współczynnik c'
  READ (UNIT=*,FMT=*) f
  pierwsze=rownanie(a=d,b=e,c=f)
  
  PRINT *, 'Podaj współczynnik a drugiego równania'
  READ (UNIT=*,FMT=*) d
  PRINT *, 'Podaj współczynnik b'
  READ (UNIT=*,FMT=*) e
  PRINT *, 'Podaj współczynnik c'
  READ (UNIT=*,FMT=*) f
  drugie=rownanie(a=d,b=e,c=f)
  
  PRINT *, 'Twój układ równań wygląda następująco:'
  PRINT *, pierwsze%a,'x+(',pierwsze%b,')y=',pierwsze%c
  PRINT *, drugie%a,'x+(',drugie%b,')y=',drugie%c
  
  Wx=licz_wyznacznik(pierwsze%c,pierwsze%b,drugie%c,drugie%b)
  Wy=licz_wyznacznik(pierwsze%a,pierwsze%c,drugie%a,drugie%c)
  W=licz_wyznacznik(pierwsze%a,pierwsze%b,drugie%a,drugie%b)
  
  PRINT *, 'Obliczone wyznaczniki:'
  PRINT *, 'Wx=',Wx
  PRINT *, 'Wy=',Wy
  PRINT *, 'W=',W
  
  x=Wx/W
  y=Wy/W
  
  PRINT *, 'Obliczone x i y:'
  PRINT *, 'x=',x
  PRINT *, 'y=',y
  STOP
END PROGRAM samodzielny_2