program main

! ------------------------------------------------------------------------------
  ! 2.1 Pseudo-random number generators
  ! Testing the linear congruential RNG with different factors.
! ------------------------------------------------------------------------------

! use congruential, only: linear_congruential

! implicit none
! integer :: size
! real :: seed, A, C, M
! real, dimension(:), allocatable :: numbers

! ! First example
! size = 100
! seed = 3
! A = 7
! C = 0
! M = 10

! ! Second example
! size = 10**4
! seed = 3
! A = 106
! C = 1283
! M = 6075

! ! Third example
! size = 10**4
! seed = 3
! A = 107
! C = 1283
! M = 6075

! ! Fourth example
! size = 10**4
! seed = 3
! A = 1103515245
! C = 12345
! M = 32768

! numbers = linear_congruential(seed, size, A, C, M)

! open(unit=20, file="main.txt", action="write", status="replace")
! write(main_file, *) numbers
! close(main_file)

! ------------------------------------------------------------------------------
  ! 2.1.1 RNG tests
  ! Using statistical tests to compare uniformity and independence of the
  ! linear congruential and intrinsic Fortran RNG.
! ------------------------------------------------------------------------------

use rng, only: linear_congruential
use rng_tests, only: chi_square, autocorrelation

implicit none
integer :: i, seed
real :: linear_chi, fortran_chi
real, dimension(10) :: expected
real, dimension(100) :: linear_numbers, fortran_numbers
real, dimension(100) :: auto_cor_linear, auto_cor_fortran

! Init seeds
seed = 3
call random_seed()

! Create random numbers
linear_numbers = linear_congruential(seed, 100, 106.0, 1283.0, 2075.0)
call random_number(fortran_numbers)

! Chi-square test
expected = (/ 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 /)
linear_chi = chi_square(linear_numbers, expected)
fortran_chi = chi_square(fortran_numbers, expected)
print*, linear_chi, fortran_chi

! Autocorrelation test
do i=1, 100
   auto_cor_linear(i) = autocorrelation(linear_numbers, i)
   auto_cor_fortran(i) = autocorrelation(fortran_numbers, i)
end do

open(unit=20, file="main.txt", action="write", status="replace")
write(20, *) linear_numbers
write(20, *) auto_cor_linear
write(20, *) fortran_numbers
write(20, *) auto_cor_fortran
close(20)

end program main
