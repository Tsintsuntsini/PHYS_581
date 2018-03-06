module rng_tests

contains

function chi_square(numbers, means) result(chi)
  implicit none

  real, dimension(:), intent(in) :: numbers
  real, dimension(:), optional :: means

  integer :: i, bins
  real :: bin_size, chi
  real, dimension(:), allocatable :: objects, observed

  ! The mean of the array is used if not specified
  if (.not. present(means)) then
     means = (/ size(numbers) /)
  end if

  bins = size(means)       ! number of steps
  bin_size = 1.0 / bins    ! step size

  allocate(objects(size(numbers)))
  allocate(observed(bins))

  do i=0, bins
     where (i * bin_size < numbers .and. numbers < i * bin_size + bin_size)
        objects = 1
     elsewhere
        objects = 0
     end where
     observed(i + 1) = sum(objects)
  end do

  print*, observed
  chi = sum((observed - means)**2 / means)

end function chi_square

function autocorrelation(numbers, interval) result(auto_cor)
  implicit none
  integer, intent(in) :: interval
  real, dimension(:), intent(in) :: numbers

  integer :: length
  real :: auto_cor, mean

  length = size(numbers)
  mean = sum(numbers) / length

  auto_cor = &
       sum((numbers(:length - interval) - mean) * (numbers(interval:) - mean)) &
       / sum((numbers(:length - interval) - mean)**2)

end function autocorrelation

end module rng_tests
