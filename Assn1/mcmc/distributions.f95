module distributions

contains

  ! ---------------------------------------------------------------------
  ! The normalized chi-squared distribution. This method is only
  ! applicable for degree k=1.
  ! ---------------------------------------------------------------------
  function chi_squared(x) result(pr)
    implicit none
    real :: PI, pr
    real, intent(in) :: x

    PI = 4 * atan(1.0)

    pr = (x**(0.5) * exp(-x / 2.0)) / (sqrt(PI) * 2.0**(0.5))

  end function chi_squared

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

  ! ---------------------------------------------------------------------
  ! The test distribution for 3.2.
  ! ---------------------------------------------------------------------
  function test(x, constant) result(pr)
    implicit none
    real :: C, pr
    real, optional :: constant
    real, intent(in) :: x

    if (.not. present(constant)) then
       C = 1.0
    else
       C = constant
    end if
    
    pr = C * x**(-2/5) * exp(- 2.0 / x)

  end function test

end module distributions
