module random_distributions
contains
  ! ---------------------------------------------------------------------
  ! Draws a random number from a Gaussian (normal) distribution.
  !
  ! Parameters:
  !     mu    : mean of the curve.
  !     sigma : standard deviation.
  ! --------------------------------------------------------------------
  function random_gaussian(mu, sigma) result(z)
    real, intent(in) :: mu, sigma

    real :: PI, z
    real, dimension(2) :: u

    PI = 4 * atan(1.0)
    call random_number(u)

    z = sqrt(- 2 * log(u(1))) * cos(2 * PI * u(2))
    z = z * sigma + mu
  end function random_gaussian

  ! --------------------------------------------------------------------
  ! Draws a random number, x, from a uniform distribution.
  !
  ! Parameters:
  !     min (optional): minimum value.
  !     max (optional): maximum value.
  ! --------------------------------------------------------------------
  function random_uniform(minimum, maximum) result(x)
    real, optional :: minimum, maximum
    real :: min, max, x

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
