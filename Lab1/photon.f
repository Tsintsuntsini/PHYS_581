PROGRAM photon_diffusion

	IMPLICIT NONE
	
	REAL :: x_i, y_i, z_i, x, y, z, p_abs, mu
	INTEGER :: num_photons, i, num_reflected, num_absorbed
	INTEGER :: n_bins(20)
	LOGICAL :: is_out, is_abs, is_refl
	
	!Initialize
	num_photons = 100
	p_abs = 1.
	num_absorbed = 0
	num_reflected = 0
	DO i=1,20
	        n_bins(i) = 0
	END DO
	
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
			CALL do_step
			x_i = x
			y_i = y
			z_i = z
		END DO

	!Count what happened to the photon. This will also help with debugging
	IF is_out THEN
	        num_reflected = num_reflected + 1
	ELSE IF is_refl THEN
	        num_absorbed = num_absorbed + 1
	ELSE
	    print*, 'ESCAPED'
	   
	   
	CONTAINS 

	FUNCTION rand_tau()
		REAL :: u
		
		u = RAND()
		rand_tau = EXP(u)
	END FUNCTION

	FUNCTION rand_phi()
		REAL :: u
		
		u = RAND()
		rand_phi = ASIN(2*u)
	END FUNCTION

	FUNCTION rand_theta()
		rand_phi = RAND()
	END FUNCTION

	SUBROUTINE do_step(x_i, y_i, z_i, x, y, z, is_out, is_abs, is_refl p_abs)
		REAL, INTENT(IN) = x_i, y_i, z_i, p_abs
		REAL, INTENT(OUT) = x, y, z, phi, theta
		LOGICAL, INTENT(OUT) :: is_out, is_abs
		REAL :: L, tau
		
		!First, figure out the random optical depth
		tau = rand_tau()
		phi = phi_rand()
		theta = theta_rand()
		
		!Determine if the photon is absorbed
		IF (RAND() < p_abs) THEN
			is_abs = .TRUE.
		
		!The photon is scattered
		ELSE 
			x = x_i + tau * SIN(theta) * COS(phi)
			y = y_i + tau * SIN(theta) * SIN(phi)
			z = z_i + tau * COS(theta)
			
			IF (z > 1) THEN
				is_out = .TRUE.
			ELSE IF (z < 0) THEN
				is_refl = .TRUE.
			END IF
		END IF
	
	END SUBROUTINE
	
END PROGRAM
