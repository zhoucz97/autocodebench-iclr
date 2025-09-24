program main
  implicit none
  integer :: result
    ! Test 1
  result = calculate_y(2)
  call assert(result .eq. 4)

  ! Test 2
  result = calculate_y(4)
  call assert(result .eq. 9)

  ! Test 3
  result = calculate_y(20)
  call assert(result .eq. 59)

  ! Additional tests
  result = calculate_y(3)
  call assert(result .eq. 7)

  result = calculate_y(19)
  call assert(result .eq. 39)

  result = calculate_y(21)
  call assert(result .eq. 62)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the value of y based on the given integer x according to a piecewise function.
! The function is defined as follows:
! y = 2x for x < 3
! y = 2x + 1 for 3 <= x < 20
! y = 3x - 1 for x >= 20

! Arguments:
! - x: An integer representing the input value.

! Returns:
! - y: The calculated integer value based on the piecewise function.

! Examples:
! - calculate_y(2) returns 4
! - calculate_y(4) returns 9
! - calculate_y(20) returns 59
integer function calculate_y(x) result(y)
    implicit none
    integer, intent(in) :: x

    if (x < 3) then
        y = 2 * x
    else if (x >= 3 .and. x < 20) then
        y = 2 * x + 1
    else
        y = 3 * x - 1
    end if
end function calculate_y

end program main