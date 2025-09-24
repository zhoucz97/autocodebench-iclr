program main
  implicit none
  integer :: result

  ! Test 1
  result = reverse_digits(127)
  call assert(result == 721)

  ! Test 2
  result = reverse_digits(305)
  call assert(result == 503)

  ! Test 3
  result = reverse_digits(450)
  call assert(result == 54)

  ! Test 4
  result = reverse_digits(999)
  call assert(result == 999)

  ! Test 5
  result = reverse_digits(100)
  call assert(result == 1)

contains


subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Reverses the digits of a given three-digit number and returns the result.
!
! Arguments:
! - number: An integer representing a three-digit number (100 <= number <= 999).
!
! Returns:
! - The reversed number as an integer.
!
! Example:
! - reverse_digits(127) returns 721
! - reverse_digits(305) returns 503
! - reverse_digits(450) returns 54
function reverse_digits(number) result(reversed_number)
    implicit none
    integer, intent(in) :: number
    integer :: reversed_number
    integer :: hundreds, tens, units

    ! Extract the hundreds, tens, and units digits
    hundreds = number / 100
    tens = (number / 10) - (hundreds * 10)
    units = number - (hundreds * 100) - (tens * 10)

    ! Reverse the digits
    reversed_number = units * 100 + tens * 10 + hundreds
end function reverse_digits

end program main