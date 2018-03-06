program lab1_random

IMPLICIT NONE

INTEGER :: A, I0, C, M, i
INTEGER :: vals(100)
REAl :: AC(100), gvals(100), rvals(100)

OPEN(15,FILE = 'randValues2.txt')

A = 107
I0 = 3
C = 1283
M = 6075

vals = pseudo(A,C,M,I0)
print *,vals

DO i=1,100
   WRITE(15,*) i, vals(i)
END DO

!Add some white space
WRITE(15,*)
WRITE(15,*)

!Determine the autocorrelation of a pseudo generator
rvals = REAL(vals)
AC = autoCorrelate(rvals, 100)

DO i=1,100
   WRITE(15,*) i, AC(i)
END DO

!Add some white space
WRITE(15,*)
WRITE(15,*)

!Determine the autocorrelation of the gfortran generator
call srand(3)
DO i=1,100
   gvals(i) = rand()
END DO

DO i=1,100
   WRITE(15,*) i, gvals(i)
END DO

!Add some white space
WRITE(15,*)
WRITE(15,*)

AC = autoCorrelate(gvals, 100)

DO i=1,100
   write(15,*) i, AC(i)
END DO

CLOSE(15)

!===============================================================

CONTAINS

FUNCTION pseudo(A,C,M,seed) result(randoms)
  integer, INTENT(IN) :: A,C,M,seed
  integer :: In1, In0, i
  integer, DIMENSION(100) :: randoms

  In0 = seed

  DO i=1,100
     In1 = MOD((A*In0 + C),M)
     print *, In1
     In0 = In1
     randoms(i) = In1
  END DO

END FUNCTION 

FUNCTION autoCorrelate(vals, N)
  REAL, INTENT(IN) :: vals(100)
  INTEGER :: k, N, i
  REAL :: mean, numerator, denominator
  REAL :: autoCorrelate(100)

  mean = SUM(vals)/100
  numerator = 0
  denominator = 0
  
  DO k = 1,100
     DO i = 1, N-k
        numerator = numerator + (vals(i) - mean) * (vals(i + k) - mean)
        denominator = denominator + (vals(i) - mean)**2
     END DO
  autoCorrelate(k) = numerator/denominator
  END DO

END FUNCTION

end program lab1_random
