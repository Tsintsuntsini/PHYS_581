module distributions
contains
  ! ---------------------------------------------------------------------
  ! Example distribution for part 4.4
  !
  ! Parameters:
  !     x: the point to be evaluated.
  ! ---------------------------------------------------------------------
  function example(x) result(pr)
    implicit none
    real, intent(in) :: x
    real :: PI, pr

    PI = 4 * atan(1.0)
    pr = (1 / (2 * sqrt(PI))) * (sin(5*x) + sin(2*x) + 2) * exp(-x**2)

  end function example

  ! ---------------------------------------------------------------------
  ! Gaussian (normal) distribution giving N(x | mean, sigma).
  !
  ! Parameters:
  !     x     : the point to find the probability of.
  !     mu    : the mean value.
  !     sigma : the standard deviation.
  ! ---------------------------------------------------------------------
  function gaussian(x, mu, sigma) result(pr)
    implicit none
    real, intent(in) :: x, mu, sigma
    real :: PI, pr

    PI = 4 * atan(1.0)
    pr = (1 / (sqrt(2 * PI) * sigma)) * exp(-(x - mu)**2 / (2 * sigma**2))
  end function gaussian

end module distributions
