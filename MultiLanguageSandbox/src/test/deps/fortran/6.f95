program main
  implicit none

  integer :: result

  result = sum_repeated_digits(2, 5)
  call assert(result == 24690)

  result = sum_repeated_digits(3, 3)
  call assert(result == 369)

  result = sum_repeated_digits(1, 4)
  call assert(result == 1234)

contains
subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the sum of a sequence where each term is a repeated digit. 
! The sequence is formed as follows: a + aa + aaa + ... + aa...a (n times).

! Arguments:
! - digit: An integer representing the digit to be repeated.
! - n: An integer representing the number of terms in the sequence.

! Returns:
! - The sum of the sequence formed by repeating 'digit' for each term up to 'n' terms.

! Example:
! - sum_repeated_digits(2, 5) returns 24690, as the sequence is 2 + 22 + 222 + 2222 + 22222.
function sum_repeated_digits(digit, n) result(sum)
    implicit none
    integer, intent(in) :: digit, n
    integer :: sum, term, i
    character(len=20) :: term_str
    
    sum = 0
    
    do i = 1, n
        ! Construct the term as a string of 'i' repeated digits
        term_str = repeat(char(digit), i)
        
        ! Convert the string back to an integer
        read(term_str, *) term
        
        ! Add the term to the sum
        sum = sum + term
    end do
end function sum_repeated_digits

end program main