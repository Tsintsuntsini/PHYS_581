PROGRAM gaussian_accept
IMPLICIT NONE

REAL :: gmax, ymin, ymax, mean, std, r1, r2, gtest
REAL :: histo(101), histo_vals(101), g_ana(101), rand_sum(1000)
INTEGER :: i, j, num_steps

OPEN(15, FILE='gauss_data.txt')

mean = 5.
std  = 1.25
num_steps = 1000000

ymin = mean - 4*std
ymax = mean + 4*std
gmax = gaussian(mean, std, mean)

!set up histogram
DO i=1,101
	histo(i) = 0
	histo_vals(i) = 0.1*i - 4*std + mean
	g_ana(i) = gaussian(mean, std, (i-1)*0.1 - 4*std + mean)
END DO

DO i=1,num_steps
	r1 = RAND() * (ymax - ymin) - ymin
	r2 = RAND() * gmax
	gtest = gaussian(mean, std, r1)
	
	IF (r2 .LE. gtest) THEN
		DO j=1,100
			IF ((r1 < histo_vals(j+1)) .AND. (r1 > histo_vals(j))) THEN
				histo(j) = histo(j) + 1
			END IF
		END DO
	END IF 
END DO

DO i=1,100
	WRITE(15,*) i, histo_vals(i), g_ana(i), histo(i)
END DO
WRITE(15,*) 101, histo_vals(101), g_ana(101), 0

!Drawing random numbers
DO i=1,1000
	rand_sum(i) = 0
	j=0
	DO WHILE (j .LE. 10)
		r1 = RAND() * (ymax - ymin) - ymin
		r2 = RAND() * gmax
		gtest = gaussian(mean, std, r1)
	
		IF (r2 .LE. gtest) THEN
			rand_sum(i) = rand_sum(i) + r1
			j = j+1
		END IF 
	
	END DO
	WRITE(15,*) rand_sum(i)
END DO

CLOSE(15)

CONTAINS

FUNCTION gaussian(mean, std, y) RESULT(g)
	REAL, INTENT(IN) :: mean, std, y
	REAL :: g
	REAL, PARAMETER :: pi=3.14159
	
	g=EXP( -(y - mean)**2 / (2 * std**2)) / (SQRT(2 * pi * std))
END FUNCTION


END PROGRAM gaussian_accept