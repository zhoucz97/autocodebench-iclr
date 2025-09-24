program main
  implicit none

  ! Test cases
  call assert(sum_or_square_sum(5, 9) == 106)
  call assert(sum_or_square_sum(3, 4) == 7)
  call assert(sum_or_square_sum(10, 0) == 10)
  call assert(sum_or_square_sum(10, 10) == 200)
contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates either the sum or the square sum of two integers based on a condition.
!
! If the sum of the squares of 'a' and 'b' (a^2 + b^2) is greater than 100, 
! the function returns the square sum. Otherwise, it returns the sum of 'a' and 'b'.
!
! Arguments:
! - a: An integer value.
! - b: An integer value.
!
! Returns:
! - The sum of 'a' and 'b' if their square sum is less than or equal to 100, 
!   otherwise their square sum.
!
! Example:
! - sum_or_square_sum(5, 9) returns 106
! - sum_or_square_sum(3, 4) returns 7

integer function sum_or_square_sum(a, b) result(res)
    implicit none
    integer, intent(in) :: a, b
    integer :: square_sum

    ! Calculate the square sum of a and b
    square_sum = a**2 + b**2

    ! Determine whether to return the sum or the square sum
    if (square_sum > 100) then
        res = square_sum
    else
        res = a + b
    end if
end function sum_or_square_sum

end program main