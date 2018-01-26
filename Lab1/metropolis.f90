PROGRAM metropolis

	IMPLICIT NONE
	REAL :: variance, x0, xn, y, del, alpha, u
	REAL, DIMENSION(:,:), ALLOCATABLE :: samples
	INTEGER :: i,j, num_steps
	
	OPEN(15, FILE = 'metropolis_data.txt')
	
	x0 = -1.
	variance = 0.025
	num_steps = 1000
	ALLOCATE( samples(2, num_steps) )
	
	CALL met_has_algorithm(num_steps, x0, samples)
	
	variance = 1.0
	
	CALL met_has_algorithm(num_steps, x0, samples)
	
	variance = 50.0
	
	CALL met_has_algorithm(num_steps, x0, samples)
	
	CLOSE(15)
	
!==============================================================
	
	CONTAINS

	FUNCTION proposal(change, stay, var) RESULT(q)
		REAL, INTENT(IN) :: change, stay, var
		REAL, PARAMETER :: pi=3.14159
		REAL :: q
		
		
		q = (1./SQRT(2*pi*var**2))*EXP(-(change-stay)**2/(2*var**2))
	END FUNCTION
	
	FUNCTION distribution(x) RESULT(p)
		REAL, INTENT(IN) :: x
		REAL :: p
		REAL, PARAMETER :: pi=3.14159265
	
		p = (1./(2*SQRT(pi))) * (SIN( 5. * x ) + SIN( 2. * x ) + 2.) * EXP( -(x**2.) )
	END FUNCTION
	
	FUNCTION accept(change, stay) RESULT(a)
		REAL, INTENT(IN) :: change, stay
		REAL :: a
	
		a = distribution(change) / distribution(stay)
	END FUNCTION
	
	SUBROUTINE met_has_algorithm(num_steps, start, samples)
		REAL, INTENT(IN) :: start
		REAL, ALLOCATABLE, INTENT(INOUT) :: samples(:,:)
		INTEGER, INTENT(IN) :: num_steps
		REAL :: del, y, alpha, u, x
		INTEGER :: i
		
		x = start
		
		DO i=1,num_steps
			del = x + (RAND() - 0.5 ) / 2   !0.125 chosen as first step size test case
			!y = proposal(del, x, variance)
			alpha = accept(del, x)
			u = RAND()
		
			IF (u < alpha) THEN
				x = del
				samples(2,i) = 1   !Accepted is true
			ELSE                
				samples(2,i) = 0   !Accepted is false
			END IF
		
			samples(1,i) = del       !Save the values for the markov sequence
		
		END DO
		
		DO i=1,num_steps
			WRITE(15,*) i, samples(1, i), samples(2, i)
		END DO
	END SUBROUTINE

END PROGRAM metropolis