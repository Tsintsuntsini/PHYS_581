module random_distributions

use distributions, only: gaussian

contains

  ! --------------------------------------------------------------------
  ! Draws a random number from a chi-squared distribution using the
  ! accept/reject method.
  ! --------------------------------------------------------------------
  function random_chi_squared(degree, minimum, maximum) result(x)
    implicit none
    integer :: k
    real, optional :: degree

    if (.not. present(degree)) then
       k = 1
    else
       k = degree
    end if

    ! TODO: find peak
    peak = 0
    trial = random_uniform(minimum, maximum)

    do while (chi_squared(trial, k) .le. random_uniform() * peak)
       trial = random_uniform(minimum, maximum)
    end do

    x = trial

  end function random_chi_squared

  ! --------------------------------------------------------------------
  ! Draws a random number from a Gaussian (normal) distribution using
  ! the accept/reject method.
  !
  ! Parameters:
  !     mu     : mean of the distribution.
  !     sigma  : standard deviation of the distribution.
  !     minimum: the minimum value that can be drawn.
  !     maximum: the maximum value that can be drawn.
  !
  ! Returns:
  !     z: random number drawn from normal distribution.
  ! --------------------------------------------------------------------
  function random_gaussian_ar(mu, sigma, minimum, maximum) result(z)
    implicit none
    real :: trial, peak, z
    real, intent(in) :: mu, sigma, minimum, maximum

    peak = gaussian(mu, mu, sigma)
    trial = random_uniform(minimum, maximum)

    do while (gaussian(trial, mu, sigma) .le. random_uniform() * peak)
       trial = random_uniform(minimum, maximum)
    end do

    z = trial

  end function random_gaussian_ar

  ! ---------------------------------------------------------------------
  ! Draws a random number from a Gaussian (normal) distribution using
  ! the Box-Mueller method.
  !
  ! Parameters:
  !     mu    : mean of the distribution.
  !     sigma : standard deviation of the distribution.
  !
  ! Returns:
  !     z: random number drawn from normal distribution.
  ! --------------------------------------------------------------------
  function random_gaussian_bm(mu, sigma) result(z)
    implicit none
    real :: PI, z
    real, intent(in) :: mu, sigma
    real, dimension(2) :: u

    PI = 4 * atan(1.0)
    call random_number(u)

    z = sqrt(- 2 * log(u(1))) * cos(2 * PI * u(2))
    z = z * sigma + mu
  end function random_gaussian_bm

  ! --------------------------------------------------------------------
  ! Draws a random number, x, from a uniform distribution. This uses the
  ! intrinsic Fortran random number generator.
  !
  ! Parameters:
  !     minimum (optional): minimum value.
  !     maximum (optional): maximum value.
  ! --------------------------------------------------------------------
  function random_uniform(minimum, maximum) result(x)
    real :: min, max, x
    real, optional :: minimum, maximum

    if (.not. present(minimum)) then
       min = 0.0
    else
       min = minimum
    end if

    if (.not. present(maximum)) then
       max = 1.0
    else
       max = maximum
    end if

    call random_number(x)

    ! Rescale x to be within specified range
    x = x * (max - min) + min

  end function random_uniform

end module random_distributions
