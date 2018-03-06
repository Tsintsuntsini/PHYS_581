PROGRAM markov_chain

  IMPLICIT NONE

  REAL, DIMENSION(2,2) :: P,P0
  REAL, DIMENSION(2,1) :: sun, rain, temp
  REAL, DIMENSION(100) :: p_sun, p_notsun, p_notrain, p_rain
  INTEGER :: i,j

  OPEN(15, FILE='markovdata.txt')

  P0 = RESHAPE((/0.9, 0.1, 0.5, 0.5 /), SHAPE(P))
  sun = RESHAPE((/1, 0/), SHAPE(sun))
  rain = RESHAPE((/0, 1/), SHAPE(rain))

  !Initialize
  p_sun(1) = 1
  p_notsun(1) = 0
  p_notrain(1) = 0
  P_rain(1) = 1
  P = MATMUL(P0, P0)
 
  ! Determine P^100
  DO i=1,99
     P = MATMUL(P,P0)
     temp = MATMUL(P, sun)
     p_sun(i+1) = temp(1,1)
     p_notsun(i+1) = temp(2,1)
     temp = MATMUL(P, rain)
     p_notrain(i+1) = temp(1,1)
     p_rain(i+1) = temp(2,1)
  END DO
  
  DO i = 1,100
     write(15,*) i, p_sun(i), p_notsun(i), p_notrain(i), p_rain(i)
  END DO

  write(15,*)
  write(15,*)

  ! Now let's invent some extreme, non-existant reality, where days can be 20% sunny, etc.

  DO i=0,10
     P = MATMUL(P0, P0)
     sun = RESHAPE((/i*0.1, 1-i*0.1/), SHAPE(sun))
     temp = MATMUL(P, sun)
     write(15,*) 'Percent sun state: ', sun(1,1)
     write(15,*) 0, temp(1,1)
     DO j=1,30
        P = MATMUL(P, P0)
        temp = MATMUL(P, sun)
        write(15,*) j, temp(1,1)
     END DO
     write(15,*)
     write(15,*)
  END DO


  CLOSE(15)

END PROGRAM markov_chain
