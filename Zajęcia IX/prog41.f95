MODULE funkcja_korzysta_z_procedury
  IMPLICIT NONE
  PUBLIC :: module_function, module_subroutine
  TYPE, PUBLIC :: nowy
    INTEGER :: j,k
  END TYPE nowy
  TYPE(nowy), PUBLIC :: x=nowy(1234,5678)
  CONTAINS
    FUNCTION module_function() RESULT (module_function_r)
    TYPE (nowy) :: module_function_r
    CALL internal_subroutine(module_function_r)
    RETURN
    
      CONTAINS
      SUBROUTINE internal_subroutine(u)
      TYPE(nowy), INTENT(OUT) :: u
      u=nowy(22,11)
      CALL module_subroutine (u%j,u%k)
      RETURN
      END SUBROUTINE internal_subroutine
    END FUNCTION module_function
    
      SUBROUTINE module_subroutine(i1,i2)
      INTEGER, INTENT (IN OUT) :: i1,i2
      INTEGER::x
      x=i1
      i1=i2
      i2=x
      RETURN
      END SUBROUTINE module_subroutine
END MODULE funkcja_korzysta_z_procedury

PROGRAM prog41
  USE funkcja_korzysta_z_procedury
  IMPLICIT NONE
  INTEGER, PARAMETER::value=789
  TYPE(nowy):: exter
  INTEGER:: inter
  exter=module_function()
  PRINT *, "EXTER=", exter
  inter=internal_function()
  PRINT *, "inter=", inter
  
  CONTAINS
    FUNCTION internal_function() RESULT(internal_function_r)
    INTEGER :: internal_function_r
    
    internal_function_r=value
    END FUNCTION internal_function
END PROGRAM prog41