program main

use random_distributions, only: random_uniform

implicit none
real :: state
integer :: i, j, n, n_draws
real, dimension(3) :: x_1
real, dimension(3, 3) :: T_2, T_n
real, dimension(4, 4) :: T_1, x_n
real, dimension(:), allocatable :: steps

call random_seed(n)

! ----------------------------------------------------------------------
! Part 2: Markov Chains
! ----------------------------------------------------------------------

! 2.1 Edged Square
! ------------------

n_draws = 550
allocate(steps(n_draws))

! Transition matrix
T_1 = reshape(                           &
    (/                                             &
          0.0   , 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0, &
       1.0 / 2.0,    0.0   , 1.0 / 2.0,    0.0   , &
       1.0 / 3.0, 1.0 / 3.0,    0.0   , 1.0 / 3.0, &
       1.0 / 2.0,    0.0   , 1.0 / 2.0,    0.0     &
    /),                                            &
    (/ 4, 4 /)                                     &
)

! Initial state
do i=1, 4
   do j=1, 4
      if (i == j) then
         x_n(i, j) = 1.0
      else
         x_n(i, j) = 0.0
      end if
   end do
end do

print*, matmul(T_1, x_n(:, 1))

open(unit=20, file='edged_square.txt')

do i=1, 4
   do n=1, n_draws
      x_n(:, i) = matmul(T_1, x_n(:, i))

      state = random_uniform()
      do j=1, size(x_n(:, i))
         if (state .le. sum(x_n(:j, i))) then
            x_n(:, i) = (/ 0.0, 0.0, 0.0, 0.0 /)
            x_n(j, i) = 1.0

            steps(n) = j
            exit
         end if
         
      end do
      
   end do

   write(20, *) steps

end do

close(20)

do n=1, 9
   T_1 = matmul(T_1, T_1)
end do
print*, matmul(T_1, x_n(:, 1))

! 2.2 Markov Processes in Business
! ----------------------------------

! Transition matrix
T_2 = transpose(reshape(  &
    (/                    &
        0.80, 0.10, 0.10, &
        0.03, 0.95, 0.02, &
        0.20, 0.05, 0.75  &
    /),                   &
    (/ 3, 3 /)            &
))

! Initial state
x_1 = (/ 0.45, 0.25, 0.30 /)

T_n = T_2
do n=2, 3
   T_n = matmul(T_2, T_n)
end do

print*, "After 3 months:", matmul(T_n, x_1)

do n=2, 100
   T_n = matmul(T_2, T_n)
end do

print*, "After 100 months:", matmul(T_n, x_1)

end program main

