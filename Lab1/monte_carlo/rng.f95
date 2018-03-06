module rng

contains

! ----------------------------------------------------------------------
  ! RNG for tau, the optical depth of the slab, which draws from the
  ! exponential distribution
  !     pr(tau) = exp(-tau)
! ----------------------------------------------------------------------
function rng_tau(x, min, max) result(tau)
  implicit none
  real, dimension(:) :: x
  real, optional :: min, max
  real, dimension(size(x)) :: tau

  if (.not. present(min)) then
     min = 0
  end if
  if (.not. present(max)) then
     max = 1
  end if
  
  call random_number(x)
  tau = -log(exp(-min) + x * (exp(-max) - exp(-min)))

end function rng_tau

! ----------------------------------------------------------------------
  ! RNG for theta, where mu = cos(theta), where theta is the azimuthal
  ! angle, which draws from the distribution
  !     pr(theta) = (1 / 2) * sin(theta)
  ! Note that the min and max parameters are the minimum and maximum
  ! values of theta, not mu.
  ! Set theta to be true if only theta is desired, otherwise mu will
  ! be returned.
! ----------------------------------------------------------------------
function rng_mu(x, min, max, theta) result(mu)
  implicit none
  real, dimension(:) :: x
  real, optional :: min, max
  logical, optional :: theta
  real :: PI
  real, dimension(size(x)) :: mu

  PI = 4 * atan(1.)

  if (.not. present(min)) then
     min = 0.0
  end if
  if (.not. present(max)) then
     max = PI
  end if

  call random_number(x)
  mu = cos(cos(min) + x * (cos(max) - cos(min)))

end function rng_mu

! ----------------------------------------------------------------------
  ! RNG for phi, the polar angle, which draws from a uniform
  ! distribution.
! ----------------------------------------------------------------------
function rng_phi(x, min, max) result(phi)
  implicit none
  real, dimension(:) :: x
  real, optional :: min, max
  real :: PI
  real, dimension(size(x)) :: phi

  PI = 4 * atan(1.)

  if (.not. present(min)) then
     min = 0
  end if
  if (.not. present(max)) then
     max = 2 * PI
  end if

  call random_number(x)
  phi = min + max * x

end function rng_phi

end module rng
