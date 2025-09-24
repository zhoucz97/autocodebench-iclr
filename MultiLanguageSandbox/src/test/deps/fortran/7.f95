program main
  implicit none
  integer, dimension(:), allocatable :: nums
  integer :: i
  integer, dimension(4):: expected
  expected = [153, 370, 371, 407]

  ! Call the function
  nums = find_narcissistic_numbers()

  do i = 1, size(nums)
    call assert(nums(i) == expected(i))
  end do
  
contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Finds all Narcissistic (or Armstrong) numbers between 100 and 999.
! A Narcissistic number is a three-digit number where the sum of the cubes of its digits equals the number itself.
! For example, 153 is a Narcissistic number because 153 = 1^3 + 5^3 + 3^3.

! Returns:
! - An array of integers containing all the Narcissistic numbers between 100 and 999.

! Example:
! - find_narcissistic_numbers() returns [153, 370, 371, 407]
function find_narcissistic_numbers() result(narcissistic_nums)
    implicit none
    integer, allocatable :: narcissistic_nums(:)
    integer :: i, num, digit1, digit2, digit3, sum_cubes, count
    
    ! First count how many narcissistic numbers exist in the range
    count = 0
    do i = 100, 999
        num = i
        digit1 = num / 100
        digit2 = (num / 10) - (digit1 * 10)
        digit3 = mod(num, 10)
        sum_cubes = digit1**3 + digit2**3 + digit3**3
        
        if (sum_cubes == num) then
            count = count + 1
        end if
    end do
    
    ! Allocate the result array with the correct size
    allocate(narcissistic_nums(count))
    
    ! Now fill the array with the actual numbers
    count = 0
    do i = 100, 999
        num = i
        digit1 = num / 100
        digit2 = (num / 10) - (digit1 * 10)
        digit3 = mod(num, 10)
        sum_cubes = digit1**3 + digit2**3 + digit3**3
        
        if (sum_cubes == num) then
            count = count + 1
            narcissistic_nums(count) = num
        end if
    end do
end function find_narcissistic_numbers

end program main