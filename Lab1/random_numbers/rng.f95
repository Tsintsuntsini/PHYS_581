module rng

contains
  function linear_congruential(seed, size, A, C, M) result (numbers)
    implicit none
    integer :: seed, size
    real :: A, C, M
    real, dimension(size) :: numbers

    integer :: i

    do i=1, size
        seed = mod(A * seed + C, M)
        numbers(i) = seed / M
    end do

    end function linear_congruential

end module rng
