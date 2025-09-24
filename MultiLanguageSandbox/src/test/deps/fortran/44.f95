program main
  implicit none
  character(len=5) :: result
  
  ! Test Case 1
  result = compare_area(5, 4, 6)
  call assert(result == 'Alice')
  
  ! Test Case 2
  result = compare_area(7, 5, 10)
  call assert(result == 'Bob')
  
  ! Additional Test Case
  result = compare_area(6, 2, 6)
  call assert(result == 'Alice')  ! Square and rectangle have the same area, Alice wins by default.

contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Compares the area of a square with side length 'a' to the area of a rectangle with sides 'b' and 'c'.
! Determines which of the two, Alice (who owns the square) or Bob (who owns the rectangle), has a larger area.

! Arguments:
! - a: An integer representing the side length of Alice's square.
! - b: An integer representing the length of Bob's rectangle.
! - c: An integer representing the width of Bob's rectangle.

! Returns:
! - A string indicating the owner of the shape with the larger area. Returns 'Alice' if the square has a larger or equal area, 'Bob' otherwise.

! Example:
! - compare_area(5, 4, 6) returns 'Alice'
! - compare_area(7, 5, 10) returns 'Bob'

character(len=5) function compare_area(a, b, c) result(winner)
    implicit none
    integer, intent(in) :: a, b, c
    character(len=5) :: winner

    ! Calculate the area of Alice's square
    integer :: alice_area
    alice_area = a * a

    ! Calculate the area of Bob's rectangle
    integer :: bob_area
    bob_area = b * c

    ! Compare the areas and determine the winner
    if (alice_area >= bob_area) then
        winner = 'Alice'
    else
        winner = 'Bob'
    end if
end function compare_area

end program main