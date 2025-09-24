program main
  implicit none
  integer, dimension(100) :: numbers
  integer :: n, result
   ! Test case 1
  n = 5
  numbers(1:5) = [100, 150, 150, 200, 250]
  result = most_frequent_element(numbers, n)
  call assert(result == 150)

  ! Test case 2
  n = 6
  numbers(1:6) = [10, 10, 20, 30, 30, 30]
  result = most_frequent_element(numbers, n)
  call assert(result == 30)

  ! Test case 3
  n = 4
  numbers(1:4) = [5, 5, 5, 10]
  result = most_frequent_element(numbers, n)
  call assert(result == 5)

  ! Test case 4
  n = 7
  numbers(1:7) = [1, 1, 1, 2, 2, 3, 3]
  result = most_frequent_element(numbers, n)
  call assert(result == 1)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Determines the most frequently occurring element in a sorted array of integers.
! If two or more elements are equally frequent, the smallest element is returned.

! Arguments:
! - numbers: An array of integers sorted in ascending order.
! - n: The number of elements in the array.

! Returns:
! - The most frequently occurring element in the array.

! Example:
! - most_frequent_element([100, 150, 150, 200, 250], 5) returns 150
integer function most_frequent_element(numbers, n) result(frequent_element)
    implicit none
    integer, intent(in) :: n
    integer, dimension(n), intent(in) :: numbers
    integer :: i, current_count, max_count, current_element, max_element
    
    if (n == 0) then
        frequent_element = 0  ! or handle error as needed
        return
    end if
    
    current_element = numbers(1)
    current_count = 1
    max_element = current_element
    max_count = current_count
    
    do i = 2, n
        if (numbers(i) == current_element) then
            current_count = current_count + 1
        else
            if (current_count > max_count) then
                max_count = current_count
                max_element = current_element
            else if (current_count == max_count .and. current_element < max_element) then
                max_element = current_element
            end if
            
            current_element = numbers(i)
            current_count = 1
        end if
    end do
    
    ! Check the last sequence
    if (current_count > max_count) then
        max_count = current_count
        max_element = current_element
    else if (current_count == max_count .and. current_element < max_element) then
        max_element = current_element
    end if
    
    frequent_element = max_element
end function most_frequent_element

end program main