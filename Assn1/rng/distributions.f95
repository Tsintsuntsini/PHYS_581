module distributions

contains

  ! ---------------------------------------------------------------------
  ! The normalized Gaussian (normal) distribution giving
  ! N(x | mu, sigma).
  !
  ! Parameters:
  !     x     : the point to find the probability of.
  !     mu    : the mean value.
  !     sigma : the standard deviation.
  !
  ! Returns:
  !     pr: the normalized value of N(x | mu, sigma)
  ! ---------------------------------------------------------------------
  function gaussian(x, mu, sigma) result(pr)
    implicit none
    real :: PI, pr
    real, intent(in) :: x, mu, sigma

    PI = 4 * atan(1.0)
    pr = (1 / (sqrt(2 * PI) * sigma)) * exp(-(x - mu)**2 / (2 * sigma**2))
  end function gaussian

end module distributions
