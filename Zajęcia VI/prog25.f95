PROGRAM p24
IMPLICIT NONE
INTEGER :: i, j
INTEGER, DIMENSION(2, 3) :: macierz_A
INTEGER, DIMENSION(3, 2) :: macierz_B
INTEGER, DIMENSION(2, 2) :: macierz_AB
INTEGER, DIMENSION(2) :: wektor_C=(/1, 2/)
INTEGER, DIMENSION(3) :: wektor_BC
macierz_A(1,1)=1
macierz_A(1,2)=2
macierz_A(1,3)=3
macierz_A(2,1)=4
macierz_A(2,2)=5
macierz_A(2,3)=6

macierz_B=TRANSPOSE(macierz_A)
DO i=1,3
PRINT *, (macierz_B(i,j), j=1,2)
END DO
macierz_AB=MATMUL(macierz_A, macierz_B)
PRINT *

DO i=1,2
PRINT *, (macierz_AB(i,j), j=1,2)
END DO

wektor_BC=MATMUL(macierz_B, wektor_C)
PRINT *
PRINT *, wektor_BC

STOP
END PROGRAM p24