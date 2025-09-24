program main
  implicit none
  integer, parameter :: size = 30
  integer :: numbers(size)
  integer :: result

  ! Test 1
  numbers(1:3) = [3, 4, 5]
  result = sum_of_primes(numbers, 3)
  call assert(result == 8)

  ! Test 2
  numbers(1:5) = [2, 3, 5, 7, 11]
  result = sum_of_primes(numbers, 5)
  call assert(result == 28)

  ! Test 3
  numbers(1:3) = [10, 20, 30]
  result = sum_of_primes(numbers, 3)
  call assert(result == 0)

contains


subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert
function is_prime(n) result(prime)
    implicit none
    integer, intent(in) :: n
    logical :: prime
    integer :: i
    
    if (n <= 1) then
        prime = .false.
        return
    else if (n == 2) then
        prime = .true.
        return
    else if (mod(n, 2) == 0) then
        prime = .false.
        return
    end if
    
    prime = .true.
    do i = 3, int(sqrt(real(n))), 2
        if (mod(n, i) == 0) then
            prime = .false.
            return
        end if
    end do
end function is_prime
! Calculates the sum of all prime numbers in a given array of integers.
!
! Arguments:
! - numbers: An array of integers.
! - count: The number of elements in the 'numbers' array.
!
! Returns:
! - The sum of all prime numbers found in the 'numbers' array.
!
! Example:
! - Given the array [3, 4, 5], the function would return 8, as 3 and 5 are prime numbers.

integer function sum_of_primes(numbers, count) result(sum)
    implicit none
    integer, intent(in) :: numbers(count)
    integer, intent(in) :: count
    integer :: i
    
    sum = 0
    do i = 1, count
        if (is_prime(numbers(i))) then
            sum = sum + numbers(i)
        end if
    end do
end function sum_of_primes

end program main