program main
  implicit none
  integer :: result

  ! Test 1: Range 2 to 3
  result = count_binary_ones_in_range(2, 3)
  call assert(result == 3)

  ! Test 2: Range 5 to 5
  result = count_binary_ones_in_range(5, 5)
  call assert(result == 2)

  ! Test 3: Range 0 to 1
  result = count_binary_ones_in_range(0, 1)
  call assert(result == 1)

  ! Test 4: Range 10 to 12
  result = count_binary_ones_in_range(10, 12)
  call assert(result == 7)

contains
subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the total number of '1's in the binary representations of all integers within a given range [l, r].
!
! Arguments:
! - l: The lower bound of the range (inclusive).
! - r: The upper bound of the range (inclusive).
!
! Returns:
! - The total number of '1's in the binary representations of all integers within the range [l, r].
!
! Example:
! - count_binary_ones_in_range(2, 3) returns 3, as the binary representations are 10 and 11, which contain three '1's in total.
integer function count_binary_ones_in_range(l, r) result(total_ones)
    implicit none
    integer, intent(in) :: l, r
    integer :: total_ones
    integer :: i, num, bit

    total_ones = 0

    do i = l, r
        num = i
        do while (num > 0)
            bit = iand(num, 1)  ! Extract the least significant bit
            if (bit == 1) then
                total_ones = total_ones + 1
            end if
            num = ishft(num, -1)  ! Right shift to check the next bit
        end do
    end do
end function count_binary_ones_in_range

end program main