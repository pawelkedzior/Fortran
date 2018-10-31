PROGRAM p24
IMPLICIT NONE
REAL :: maksymalna, minimalna, il_skal
INTEGER, DIMENSION(2, 3) :: macierz_A
INTEGER, DIMENSION(3, 2) :: macierz_B
INTEGER, DIMENSION(2, 2) :: macierz_AB
INTEGER, DIMENSION(3) :: wektor_C
INTEGER, DIMENSION(3) :: wektor_D
INTEGER, DIMENSION(6) :: wektor_E
INTEGER, DIMENSION(1:2) :: indeksy

macierz_A(1,1)=1; macierz_A(1,2)=2; macierz_A(1,3)=3
macierz_A(2,1)=4
macierz_A(2,2)=5
macierz_A(2,3)=6

maksymalna=MAXVAL(macierz_A)
indeksy=MAXLOC(macierz_A)
PRINT *, "maksimum macierzy A znajduje się w (", indeksy(1),",", indeksy(2),")", maksymalna

minimalna=MINVAL(macierz_A)
indeksy=MINLOC(macierz_A)
PRINT *, "minimum macierzy A znajduje się w (", indeksy(1),",", indeksy(2),")", minimalna

wektor_E=PACK(macierz_A, .TRUE.)
PRINT *
PRINT *, "wektor_E=", wektor_E

wektor_C=PACK(macierz_A, macierz_A<=3)
wektor_D=PACK(macierz_A, macierz_A>3)
il_skal=DOT_PRODUCT(wektor_C, wektor_D)

PRINT *
PRINT *, "wektor_C=", wektor_C
PRINT *, "wektor_D=", wektor_D
PRINT *, "iloczyn skalarny wektorów C i D jest równy ", il_skal

STOP
END PROGRAM p24