program main
  implicit none

  ! Test cases
  call assert(smallest_perfect_square_greater_than_or_equal_to(71711) == 71824)
  call assert(smallest_perfect_square_greater_than_or_equal_to(16) == 16)
  call assert(smallest_perfect_square_greater_than_or_equal_to(17) == 25)
  call assert(smallest_perfect_square_greater_than_or_equal_to(1) == 1)
  call assert(smallest_perfect_square_greater_than_or_equal_to(99999) == 100489)


contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the smallest perfect square that is greater than or equal to the given integer n.
! A perfect square is a number that can be expressed as the square of an integer.

! Arguments:
! - n: An integer value.

! Returns:
! - The smallest perfect square greater than or equal to n.

! Example:
! - smallest_perfect_square_greater_than_or_equal_to(71711) returns 71824
function smallest_perfect_square_greater_than_or_equal_to(n) result(square)
    implicit none
    integer, intent(in) :: n
    integer :: square
    integer :: root
    
    ! Calculate the integer square root of n (floor of sqrt(n))
    root = floor(sqrt(real(n)))
    
    ! Check if n is already a perfect square
    if (root * root == n) then
        square = n
    else
        ! Otherwise, the next perfect square is (root + 1)^2
        square = (root + 1) ** 2
    end if
end function smallest_perfect_square_greater_than_or_equal_to

end program main