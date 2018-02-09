program main

! ----------------------------------------------------------------------
! Part 3: Markov Chain Monte Carlo
! ----------------------------------------------------------------------
use distributions, only: gaussian, test
use random_distributions, only: &
    random_chi_squared, random_gaussian_ar, random_uniform

implicit none
real :: delta, time_begin, time_end, trial, x_0, x_n
integer :: n, n_steps
real, dimension(:), allocatable :: steps

! 3.1 Metropolis Algorithm: Random Walk
! ---------------------------------------

! TODO: Correct sigma
mu = 0.0
sigma = 0.2

! Metropolis algorithm
call cpu_time(begin_time)

n_steps = 5000
allocate(steps(n_steps)

x_0 = 0.0
do n=1, n_steps
   trial = x_0 + random_uniform() * delta

   accept = min(1.0, gaussian(trial, mu, sigma) / gaussian(x_n, mu, sigma))
   if (random_uniform() < accept) then
      x_0 = trial
   end if

   steps(n) = x_0

end do

call cpu_time(time_end)
print*, "Metropolis took", time_end - time_begin, "seconds"

open(unit=20, file="random_walk.txt")
write(20, *) steps
close(20)

! Monte Carlo algorithm
call cpu_time(time_begin)

a = mu - 4 * sigma
b = mu + 4 * sigma

n_draws = 10**6
allocate(ar_draws(n_draws))

do n=1, n_steps
   steps(n) = random_gaussian_ar(mu, sigma, a, b)
end do

call cpu_time(time_end)
print*, "Monte Carlo took", time_end - time_begin, "seconds"

! 3.2 Metropolis Algorithm: Mixed Chains
! ----------------------------------------
n_steps = 500

! Uniform proposal distribution
x_0 = 1.0
do n=1, n_steps
   trial = x_0 + random_uniform(minimum=0.0, maximum=100.0)

   accept = min(1.0, test(trial) / test(x_0))
   if (random_uniform() < accept) then
      x_0 = trial
   end if
 
   steps(n) = x_0

end do

open(unit=30, file='uniform.txt')
write(30, *) steps
close(30)

! Chi-squared proposal distribution
x_0 = 1.0
do n=1, n_steps
   trial = x_0 + random_chi_squared()

   accept = min(1.0, chi_squared(trial) / chi_squared(x_0))
   if (random_uniform() < accept) then
      x_0 = trial
   end if

   steps(n) = x_0

end do

open(unit=40, file='chi_squared.txt')
write(40, *) steps
close(40)

end program main
