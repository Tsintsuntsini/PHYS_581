PROGRAM square
IMPLICIT NONE

REAL :: trans(4,4), r, psum
INTEGER :: draws, i, j, k, seed, cstate, state(4,4)

OPEN(15, FILE='markovsquare.txt')

trans = RESHAPE( (\ 0., 0.5, 0.33, 0.5, 0.33, 0., 0.33, 0, 0.33, 0.5, 0., 0.5, 0.33, 0., 0.44, 0.\), (/4,4/) )
state = RESHAPE( (\1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 \), (\4,4\) )

seed = 3344223
CALL SRAND(seed)

DO i=1,4
	cstate = i
	DO j=1,draws
		r = RAND()
		psum = 0
		DO k=1,4
			IF (r .LE. trans(cstate, k)) THEN
				cstate = k
				psum = psum + trans(cstate, k)
			END IF
		IF (j .GE. 50) THEN
			WRITE(15,*) cstate
		END IF
	END DO
END DO

CLOSE(15)

END PROGRAM square