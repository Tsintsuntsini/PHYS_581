PROGRAM rand_walk
IMPLICIT NONE

REAL :: mean, dev, x0, x, proposed, r, accept, x_sum, r1, r2, gmax, ymin, ymax, time_begin, time_end, gtest
INTEGER :: i, j, num_steps, num_accepted

OPEN(15, FILE='rand_walk.txt')

!Initializing variables
mean = 0.                           !Mean of the gaussian 
dev = SQRT(5./2.)                   !STD of the gaussian by definition
x0 = 0.0                            !Starting position
num_steps = 5000                    !Number of points to sample in the distribution (1pt = 1 step)
x_sum = 0.                          !running sum of x values
num_accepted = 0                    !Number of points created for the distribution in accept/reject
gmax = gaussian(mean, dev, mean)    !Maximum value of gaussian
ymin = mean - 4*dev                 !Minimum gaussian x_coordinate
ymax = mean + 4*dev                 !Maximum gaussian x_corrdinate

CALL CPU_TIME(time_begin)           !Track MCMC time
x = x0
DO i=1,num_steps
	proposed = x + uniform(dev)        !Propose a move
	r = RAND()
	accept = omega(proposed)/omega(x)  !Determine acceptance coefficient
	IF (r .LT. accept) THEN         !If accepted, change current x position
		x = proposed
	END IF
	x_sum = x_sum + x               !Keep tally of sum
	WRITE(15,*) x, x_sum/FLOAT(i)
END DO
CALL CPU_TIME(time_end)

PRINT*, time_end - time_begin

CALL CPU_TIME(time_begin)           !Track accept/reject time
num_steps = 0
DO WHILE (num_steps < 5000)            !5000 samples not 5000 tests
	r1 = RAND() * (ymax - ymin) - ymin
	r2 = RAND() * gmax
	gtest = gaussian(mean, dev, r1)
	
	IF (r2 .LE. gtest) THEN         !If random point under the curve
		num_steps = num_steps + 1   !One more accepted
		WRITE(15,*) r1
	END IF 
END DO
CALL CPU_TIME(time_end)

PRINT*, time_end - time_begin


CLOSE(15)

CONTAINS 

FUNCTION omega(y) RESULT(g)
	REAL, INTENT(IN) :: y
	REAL :: g
	
	g = EXP(-0.2 * y**2)
END FUNCTION

FUNCTION uniform(dev) RESULT(g)
	REAL :: g
	REAL, INTENT(IN) :: dev
	g = 2 * dev * RAND() - dev
END FUNCTION

FUNCTION gaussian(mean, std, y) RESULT(g)
	REAL, INTENT(IN) :: mean, std, y
	REAL :: g
	REAL, PARAMETER :: pi=3.14159
	
	g=EXP( -(y - mean)**2 / (2 * std**2)) / (SQRT(2 * pi * std))
END FUNCTION

END PROGRAM