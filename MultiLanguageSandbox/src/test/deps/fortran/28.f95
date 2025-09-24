program main
  implicit none
  call assert(is_palindrome("12321")  .eqv. .true.)
  call assert(is_palindrome("12345")  .eqv. .false.)
  call assert(is_palindrome("44")     .eqv. .true.)
  call assert(is_palindrome("2332")   .eqv. .true.)
  call assert(is_palindrome("3")      .eqv. .true.)
  call assert(is_palindrome("123321") .eqv. .true.)
  call assert(is_palindrome("1234321").eqv. .true.)
  call assert(is_palindrome("12345432").eqv. .false.)


contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Checks if a given number (represented as a string) is a palindrome.
! A palindrome is a sequence of characters that reads the same forward and backward.

! Arguments:
! - number_string: A string representing a positive integer.

! Returns:
! - A logical value indicating whether the number is a palindrome (.true.) or not (.false.).

! Example:
! - is_palindrome("12321") returns .true.
! - is_palindrome("12345") returns .false.
logical function is_palindrome(number_string)
    character(len=*), intent(in) :: number_string
    integer :: i, n
    
    n = len(number_string)
    
    do i = 1, n/2
        if (number_string(i:i) /= number_string(n-i+1:n-i+1)) then
            is_palindrome = .false.
            return
        end if
    end do
    
    is_palindrome = .true.
end function is_palindrome

end program main