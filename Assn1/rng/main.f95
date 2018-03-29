program main

! ----------------------------------------------------------------------
! Part 1: General Problems involving RNG
! ----------------------------------------------------------------------

use random_distributions, only: random_gaussian_ar, random_uniform

implicit none

real :: a, b, mu, sigma
integer :: day, i, n, n_draws, n_matches, n_trials
integer, dimension(30) :: birthdays
real, dimension(:), allocatable :: ar_draws, clt_draws

call random_seed()

! 1.1 Birthday Problem
! ----------------------
n_trials = 10000

do n=1, n_trials
   birthdays = 0.0 * birthdays

   ! Pick random birthdays
   do i=1, size(birthdays)
      day = int(random_uniform(minimum=1.0, maximum=366.0))

      ! End experiment if someone already has that birthday
      if (any(birthdays == day)) then
         n_matches = n_matches + 1
         exit
      else
         birthdays(i) = day
      end if

   end do

end do

print*, "Matching birthdays:", real(n_matches) / n_trials
print*, "Number of trials:", n_trials

! 1.2 Random Numbers from a Normal Distribution
! -----------------------------------------------
mu = 5.0
sigma = 1.25

a = mu - 4 * sigma
b = mu + 4 * sigma

! Random draw with accept/reject method
n_draws = 10**6
allocate(ar_draws(n_draws))

do n=1, n_draws
   ar_draws(n) = random_gaussian_ar(mu, sigma, a, b)
end do

! Random draw with central limit theorem
n_draws = 10**4
allocate(clt_draws(n_draws))

do n=1, n_draws

   ! Each draw is sum of ten random numbers
   clt_draws(n) = random_uniform()
   do i=2, 10
      clt_draws(n) = clt_draws(n) + random_uniform()
   end do

   clt_draws(n) = clt_draws(n)

end do


open(unit=20, file="normal.txt")
write(20, *) ar_draws
write(20, *) clt_draws
close(20)

end program main
