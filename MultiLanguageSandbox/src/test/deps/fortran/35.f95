program main
  implicit none
  integer :: result

  ! Test 1
  result = count_ones(15)
  call assert(result == 8)

  ! Test 2
  result = count_ones(20)
 
  call assert(result == 12)

  ! Test 3
  result = count_ones(100)
 
  call assert(result == 21)

  ! Test 4
  result = count_ones(5)

  call assert(result == 1)

  ! Test 5
  result = count_ones(30000)
  call assert(result == 22000) ! This value needs to be calculated for correctness


contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Counts the number of times the digit '1' appears in all numbers from 1 to n.
!
! Arguments:
! - n: A positive integer (n <= 30000).
!
! Returns:
! - The count of the digit '1' from 1 to n.
!
! Example:
! - count_ones(15) returns 8, as '1' appears in 1, 10, 11 (twice), 12, 13, 14, 15.

integer function count_ones(n) result(count)
    implicit none
    integer, intent(in) :: n
    integer :: count
    integer :: i, num, digit

    count = 0

    do i = 1, n
        num = i
        do while (num > 0)
            digit = mod(num, 10)
            if (digit == 1) then
                count = count + 1
            end if
            num = num / 10
        end do
    end do
end function count_ones

end program main