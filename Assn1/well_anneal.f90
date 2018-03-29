PROGRAM well_anneal
IMPLICIT NONE

REAL :: T_start, x_start
INTEGER :: T_steps, channel, seed

channel = 15

OPEN(channel, FILE='well_anneal.txt')

T_start = 1.
x_start = 0.
T_steps = 1
seed = 999256

CALL SRAND(seed)
CALL anneal(x_start, T_start, T_steps, 1000, channel)

T_start = 0.4
CALL anneal(x_start, T_start, T_steps, 1000, channel)

T_start = 0.1
CALL anneal(x_start, T_start, T_steps, 1000, channel)

T_start = 1.0
T_steps = 1000
CALL anneal(x_start, T_start, T_steps, 1, channel)

CLOSE(channel)

!==============================================================================
CONTAINS

SUBROUTINE anneal(initial_x, initial_t, num_steps, steps_at_T, channel)
	!Finds some local minimum of cost according to simulated annealing
	!	initial_x  :: x position at which to start the simulation
	!   initial_T  :: temperature of the simulation at t=0
	!   num_steps  :: number of times to decrease the temperature
	!	steps_at_T :: how many random steps are attempted at each temperature step
	!   channel    :: file number open
	REAL, INTENT(IN) :: initial_x, initial_t
	INTEGER, INTENT(IN) :: num_steps, steps_at_T, channel
	REAL :: x, minimum_val, minimum, T, x_proposed
	INTEGER :: i, j
	LOGICAL :: move
	
	minimum_val = V(initial_x)                         !c to V
	minimum = initial_x
	
	T = initial_t
	
	x = initial_x
	DO i=1,num_steps
		DO j=1,steps_at_T
			x_proposed = x + rand_step()
			move = do_move(x, x_proposed, T)
			IF (V(x_proposed) < V(x) ) THEN          !c to V
				x = x_proposed
				IF (V(x) < minimum_val) THEN       !c to V
					minimum_val = V(x)
					minimum = x
				END IF
			ELSE IF ( move .EQV. .TRUE. ) THEN
				x = x_proposed
			END IF
			WRITE(channel, *) x, minimum, minimum_val, T
		END DO
		T = new_temp(i)
	END DO
	
END SUBROUTINE
	
FUNCTION V(x) RESULT(p)                              !IN THIS EXAMPLE V IS OUR COST FUNCTION
	REAL, INTENT(IN) :: x
	REAL :: p
	
	p = x**4 - x**2 + 0.1*x
END FUNCTION

FUNCTION rand_step() RESULT(r)
	REAL :: r
	
	r = 2. * RAND() - 1.
END FUNCTION

FUNCTION do_move(y, y_proposed, temp) RESULT(d)
	REAL, INTENT(IN) :: y, y_proposed, temp
	LOGICAL :: d
	REAL :: p , u
	
	p = EXP( -(V(y_proposed) - V(y)) / temp )
	u = RAND()
	
	IF (u .LT. p) THEN
		d = .TRUE.
	ELSE
		d = .FALSE.
	END IF
END FUNCTION

FUNCTION new_temp(i) RESULT(t)
	INTEGER, INTENT(IN) :: i
	REAL :: t
	
	t = 1. - 0.001 * i
END FUNCTION
END PROGRAM