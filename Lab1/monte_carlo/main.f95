program main

use rng, only: rng_tau, rng_mu, rng_phi

! 3.6 Test RNG
implicit none
real :: PI
real, dimension(10**6) :: tau, mu, phi

PI = 4 * atan(1.)

tau = rng_tau(tau, min=0., max=1.)
mu = rng_mu(mu, min=0., max=PI)
phi = rng_phi(phi, min=0., max=PI)

open(unit=20, file="rng.txt")
write(20, *) tau
write(20, *) mu
write(20, *) phi
close(20)

end program main
