PROGRAM photon_diffusion

	IMPLICIT NONE
	
	REAL :: x_i, y_i, z_i, x, y, z, p_abs, mu, tau, phi, tmax, theta
	INTEGER :: num_photons, i, j, num_reflected, num_absorbed
	LOGICAL :: is_out, is_abs, is_refl

    OPEN(15, FILE='histograms_2.txt')
	OPEN(16, FILE='photon_output_2.txt')
	
	!Initialize
	num_photons = 1000000000
	p_abs = 0.5
	num_absorbed = 0
	num_reflected = 0
	tmax = 10
	
!    DO i=1,1000000
!       tau = rand_tau(tmax)
!       mu = rand_theta()
!       phi = rand_phi()
!       write(15,*) tau, mu, phi
!    END DO
  
	!For each photon
	DO i=1,num_photons
		x_i = 0
		y_i = 0
		z_i = 0
		is_out = .FALSE.
		is_abs = .FALSE.
		is_refl = .FALSE.
		
		!Follow the path for each photon
		DO WHILE (.NOT. (is_out .OR. is_abs .OR. is_refl))
			CALL do_step(x_i, y_i, z_i, tmax, p_abs, x, y, z, theta, is_out, is_abs, is_refl)
			x_i = x
			y_i = y
			z_i = z
		END DO

		!Count what happened to the photon. This will also help with debugging
		IF (is_refl .EQV. .TRUE.) THEN
				num_reflected = num_reflected + 1
		ELSE IF (is_abs .EQV. .TRUE.) THEN
				num_absorbed = num_absorbed + 1
		ELSE
			mu = COS(theta)
			WRITE(16,*) mu
		END IF   
	
	END DO
	
	PRINT*, num_reflected, num_absorbed
	
    CLOSE(15)
	CLOSE(16)
	
!=============================================================================
	   
CONTAINS 

	FUNCTION rand_tau(tmax) RESULT(r)
		REAL :: u,r
		REAL, INTENT(IN) :: tmax
		
		u = RAND()
		r = -LOG(1-u*(1-EXP(-tmax)))
	END FUNCTION

	FUNCTION rand_theta() RESULT(r)
		REAL :: u,r
		
		u = RAND()
		r = ACOS(1-2*u)
	END FUNCTION

	FUNCTION rand_phi() RESULT(r)
		REAL :: u,r
            REAL, PARAMETER :: pi=3.14159
            u = RAND()
		r = 2.*pi*u
	END FUNCTION

	SUBROUTINE do_step(x_i, y_i, z_i, tmax, p_abs, x, y, z, theta, is_out, is_abs, is_refl)
		REAL, INTENT(IN) :: x_i, y_i, z_i, p_abs, tmax
		REAL, INTENT(OUT) :: x, y, z, theta
		LOGICAL, INTENT(INOUT) :: is_out, is_abs, is_refl
		REAL :: L, tau, phi
		
		!First, figure out the random optical depth
		tau = rand_tau(tmax)
		phi = rand_phi()
		theta = rand_theta()
		
		!Determine if the photon is absorbed
		IF (RAND() < p_abs) THEN
			is_abs = .TRUE.
		
		!The photon is scattered
		ELSE 
			x = x_i + tau * SIN(theta) * COS(phi)
			y = y_i + tau * SIN(theta) * SIN(phi)
			z = z_i + tau * COS(theta)
			
			IF (z > 10) THEN
				is_out = .TRUE.
			ELSE IF (z < 0) THEN
				is_refl = .TRUE.
			END IF
		END IF
	
	END SUBROUTINE
	
END PROGRAM
