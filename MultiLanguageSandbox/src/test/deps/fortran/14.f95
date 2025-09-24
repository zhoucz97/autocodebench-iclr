program main
  implicit none
  integer, dimension(10) :: apple_heights
  integer, dimension(5) :: apple_heights2
  integer :: num_unreachable

  ! Initialize the apple heights
  apple_heights = [100, 200, 150, 140, 129, 134, 167, 198, 200, 111]

  ! Call the function and test
  num_unreachable = count_unreachable_apples(apple_heights, 10, 110)
  call assert(num_unreachable == 5)


  ! Initialize the apple heights
  apple_heights2 = [432, 156, 123, 999, 421]

  ! Call the function and test
  num_unreachable = count_unreachable_apples(apple_heights2, 5, 150)
  call assert(num_unreachable == 3)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Counts the number of apples that cannot be reached by TaoTao.
! TaoTao can reach apples within her maximum reach height, and she has a 30cm high stool to assist.
!
! Arguments:
! - apple_heights: An array containing the heights of the apples from the ground (in centimeters).
! - num_apples: The total number of apples on the tree.
! - max_reach: TaoTao's maximum reach height without the stool (in centimeters).
!
! Returns:
! - The number of apples that TaoTao cannot reach, even with the stool.
!
! Example:
! - count_unreachable_apples([100, 200, 150, 140, 129, 134, 167, 198, 200, 111], 10, 110) returns 5
integer function count_unreachable_apples(apple_heights, num_apples, max_reach) result(unreachable)
    implicit none
    integer, intent(in) :: num_apples, max_reach
    integer, dimension(num_apples), intent(in) :: apple_heights
    integer :: unreachable, i

    ! Initialize the count of unreachable apples
    unreachable = 0

    ! Loop through each apple to check if it is reachable
    do i = 1, num_apples
        if (apple_heights(i) > max_reach + 30) then
            unreachable = unreachable + 1
        end if
    end do

end function count_unreachable_apples

end program main