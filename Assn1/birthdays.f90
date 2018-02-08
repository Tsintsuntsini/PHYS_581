PROGRAM birthdays
IMPLICIT NONE

INTEGER :: bdays(30), i, j, k, success, seed

PRINT*, "Input an integer seed"
READ(*,*) seed
CALL SRAND(seed)

success=0
DO k=1,10000
	DO i=1,30
		bdays(i) = FLOOR(RAND()*365)
	END DO

	b_testing:DO i=1,30
		DO j=i+1,30
			IF (bdays(i) == bdays(j)) THEN
				success = success + 1
				EXIT b_testing
			END IF
		END DO
	END DO b_testing
END DO

PRINT*, success
END PROGRAM birthdays