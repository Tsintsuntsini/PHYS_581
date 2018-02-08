PROGRAM rand_walk
IMPLICIT NONE

REAL :: mean, udev, x0, x, proposed, r, accept
INTEGER :: i, j, num_steps, num_accepted

OPEN(15, FILE='2chains.txt')

!Initializing variables
mean = 0.                           !Mean of the gaussian 
udev = 100.                         !Max value to be sampled from uniform or chi squared
x0 = 1.0                            !Starting position
num_steps = 500                     !Number of points to sample in the distribution (1pt = 1 step)
num_accepted = 0

!Using a uniform sample
x = x0
DO i=1,num_steps
	proposed = x + uniform(udev)    !Propose a move
	r = RAND()
	accept = P(proposed)/P(x)       !Determine acceptance coefficient
	IF (r .LT. accept) THEN         !If accepted, change current x position
		x = proposed
		num_accepted = num_accepted + 1
	END IF
	WRITE(15,*) x
END DO
PRINT*, FLOAT(num_accepted) / FLOAT(num_steps)

!Using a chi squared sample
x = x0
x = x0
num_accepted = 0
DO i =1, num_steps
	proposed = x + chi_sample(udev)           !Propose a move
	accept = P(proposed) / P(x)           !Determine acceptance coefficient - chi sq. requires metro algorithm without hastings
	r = RAND()
	IF (r .LT. accept) THEN               !If accepted, change current x position
		x = proposed
		num_accepted = num_accepted + 1
	END IF
	WRITE(15,*) x
END DO
PRINT*, FLOAT(num_accepted) / FLOAT(num_steps)

CLOSE(15)

CONTAINS 

FUNCTION P(y) RESULT(g)
	REAL, INTENT(IN) :: y
	REAL :: g
	
	g = y**(-5./2.)*EXP(-2./y)
END FUNCTION

FUNCTION uniform(dev) RESULT(g)
	REAL :: g
	REAL, INTENT(IN) :: dev
	
	g = dev * RAND()
END FUNCTION

FUNCTION chi(y) RESULT(g)
	REAL :: g
	REAL, INTENT(IN) :: y
	REAL, PARAMETER :: pi=3.14159
	
	g = y**(-1./2.) * EXP(-y/2.) / (SQRT(pi) * 2.**(1./2.) )
END FUNCTION

FUNCTION chi_sample(d) RESULT(g)
	REAL :: g, r1, r2, gtest, gmax
	REAL, INTENT(IN) :: d
	
	r2 = 1.
	gtest = 0.
	gmax = chi(4./5.)
	
	DO WHILE (r2 .GT. gtest)
		r1 = uniform(d)
		gtest = chi(r1)
		r2 = uniform(gmax)
	END DO
	
	g = r1
END FUNCTION

FUNCTION gaussian(mean, std, y) RESULT(g)
	REAL, INTENT(IN) :: mean, std, y
	REAL :: g
	REAL, PARAMETER :: pi=3.14159
	
	g=EXP( -(y - mean)**2 / (2 * std**2)) / (SQRT(2 * pi * std))
END FUNCTION

END PROGRAM