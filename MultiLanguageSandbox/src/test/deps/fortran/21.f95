program main
  implicit none

  ! Test cases
  call assert(sum_unrelated_to_two(8) == 16)
  call assert(sum_unrelated_to_two(10) == 25)
  call assert(sum_unrelated_to_two(20) == 100)
  call assert(sum_unrelated_to_two(2) == 1)
  call assert(sum_unrelated_to_two(15) == 64)

  print *, "All tests passed successfully."

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! This function calculates the sum of all positive integers less than or equal to 'n' that are not related to the number 2.
! A number is considered unrelated to 2 if it is not divisible by 2 and does not contain the digit 2 in its decimal representation.

! Arguments:
! - n: An integer in the range of 2 to 1000.

! Returns:
! - The sum of all positive integers less than or equal to 'n' that are unrelated to the number 2.

! Example:
! - sum_unrelated_to_two(8) returns 16, as the numbers unrelated to 2 are 1, 3, 5, and 7.
function sum_unrelated_to_two(n) result(sum)
    implicit none
    integer, intent(in) :: n
    integer :: sum
    integer :: i, num
    
    sum = 0
    
    do i = 1, n
        ! Check if the number is not divisible by 2
        if (mod(i, 2) /= 0) then
            num = i
            ! Check if the number contains the digit 2
            do while (num > 0)
                if (mod(num, 10) == 2) exit
                num = num / 10
            end do
            if (num == 0) then
                sum = sum + i
            end if
        end if
    end do
end function sum_unrelated_to_two

end program main