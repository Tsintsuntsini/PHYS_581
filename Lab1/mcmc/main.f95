program main
! ! 4.1.1

! ! 4.1.2
! implicit none
! integer :: n
! real, dimension(2, 1) :: sunny, sunny_n, rainy, rainy_n
! real, dimension(2, 2) :: P, P_n

! ! Initial conditions
! sunny = transpose((/ 1, 0 /))
! rainy = transpose((/ 0, 1 /))

! ! Markov transformation
! P = reshape((/ 0.9, 0.1, 0.5, 0.5 /), (/ 2, 2/))
! P_n = reshape((/ 1, 0, 0, 1 /), (/ 2, 2 /))

! ! Apply markov chain
! do n=1, 100
!    P_n = matmul(P, P_n)
!    sunny_n = matmul(P_n, sunny)
!    rainy_n = matmul(P_n, rainy)
!    write(20, *) sunny_n, rainy_n
! end do
! print*, P_n


! ! 4.4
! use distributions, only: example, gaussian
! use random_distributions, only: random_gaussian, random_uniform
! implicit none
! integer :: i, n, n_accept, N_STEPS
! real :: accept, x_0, x_p
! real, dimension(3):: SIGMA
! ! real, dimension(1000) :: walk
! real, dimension(50000) :: walk

! ! Constants
! SIGMA = (/ 0.0025, 1.0, 50.0 /)
! ! N_STEPS = 1000
! N_STEPS = 50000

! ! Target distribution: p = example(x)
! ! Proposal distribution: q = gaussian(x | mean, std_dev)
! ! open(unit=20, file="mcmc1.txt")
! open(unit=20, file='mcmc2.txt')


! do i=1, 3
!   ! Starting variables
!   x_0 = -1.0
!   n_accept = 0

!   do n=1, N_STEPS
!     ! Proposal
!     x_p = random_gaussian(x_0, SIGMA(i))

!     ! Acceptance probability
!     accept = min( 1.0, &
!           (example(x_p) * gaussian(x_p, x_0, SIGMA(i))) &
!           / (example(x_0) * gaussian(x_0, x_p, SIGMA(i))))

!     if (random_uniform() < accept) then
!         x_0 = x_p
!         n_accept = n_accept + 1
!     end if

!     ! Update step
!     walk(n) = x_0
!   end do
!   write(20, *) walk, real(n_accept) / N_STEPS
! end do

! close(20)

! 4.4.1 Burn-in phase
use distributions, only: example, gaussian
use random_distributions, only: random_gaussian, random_uniform
implicit none

integer :: burn, n, n_accept, n_steps, n_walkers, w
real :: accept, sigma, x_0, x_p
real, dimension(1000) :: walk

sigma = 0.2
n_walkers = 3
n_steps = 1000
burn = 0
! burn = 200

open(unit=20, file='burn1.txt')
! open(unit=20, file='burn2.txt')

do w=1, n_walkers
  x_0 = -3.0
  n_accept = 0
  do n=1, n_steps
    x_p = random_gaussian(x_0, sigma)

    accept = min(1.0, &
         (example(x_p) * gaussian(x_p, x_0, sigma)) &
         / (example(x_0) * gaussian(x_0, x_p, sigma)))

    if (random_uniform() < accept) then
       x_0 = x_p
       if (n > burn) then
          n_accept = n_accept + 1
       end if
    end if

    walk(n) = x_0
  end do
  write(20, *) walk(burn + 1:), real(n_accept) / (n_steps - burn)
end do

close(20)

end program main


