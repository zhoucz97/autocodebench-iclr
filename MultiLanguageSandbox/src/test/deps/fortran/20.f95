program main
  implicit none
  integer, parameter :: max_size = 200
  integer :: test_array(max_size), sorted_array(max_size), n
  integer :: i

  ! Test case 1
  n = 5
  test_array = 0
  test_array(1:n) = [8, 3, 6, 4, 9]
  call sort_array(test_array, n, sorted_array)

  ! Validate the sorted array
  call assert(all(sorted_array(1:n) == [3, 4, 6, 8, 9]))


  ! Test case 1
  n = 10
  test_array = 0
  test_array(1:n) = [112, 223, 5443, 2, 435, 8, 3, 6, 4, 9]
  call sort_array(test_array, n, sorted_array)

  ! Validate the sorted array
  call assert(all(sorted_array(1:n) == [2, 3, 4, 6, 8, 9, 112, 223, 435, 5443]))

contains


subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Sorts a given array of integers in ascending order.
!
! This subroutine takes an array of integers and its size as input and returns a new array 
! that contains the elements of the input array sorted in ascending order. The sorting is 
! done using a simple sorting algorithm, such as bubble sort or insertion sort.
!
! Arguments:
! - numbers: An array of integers, each with an absolute value less than 10000.
! - n: The size of the input array.
! - sorted_numbers: An array of integers that will contain the sorted elements.
!
! Examples:
! - Given numbers = [8, 3, 6, 4, 9] and n = 5, the subroutine will output sorted_numbers = [3, 4, 6, 8, 9].


subroutine sort_array(numbers, n, sorted_numbers)
    implicit none
    integer, intent(in) :: n
    integer, dimension(n), intent(in) :: numbers
    integer, dimension(n), intent(out) :: sorted_numbers
    integer :: i, j, temp
    
    ! Copy the input array to the output array
    sorted_numbers = numbers
    
    ! Bubble sort algorithm
    do i = 1, n-1
        do j = 1, n-i
            if (sorted_numbers(j) > sorted_numbers(j+1)) then
                ! Swap elements
                temp = sorted_numbers(j)
                sorted_numbers(j) = sorted_numbers(j+1)
                sorted_numbers(j+1) = temp
            end if
        end do
    end do
end subroutine sort_array
end program main