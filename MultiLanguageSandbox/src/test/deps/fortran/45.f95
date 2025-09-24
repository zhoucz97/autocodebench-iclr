program main
  implicit none
  integer, parameter :: max_m = 3
  integer :: result(max_m), size, n, m

  ! Test case 1
  n = 6
  m = 3
  call max_product_distribution(n, m, result, size)
  call assert(all(result == [2, 2, 2]))

  ! Test case 2
  n = 8
  m = 3
  call max_product_distribution(n, m, result, size)
  call assert(all(result == [2, 3, 3]))
contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Finds M positive integers that sum up to N and have the maximum possible product.
! The output is arranged in lexicographically smallest order.

! Arguments:
! - n: The sum that the M integers should equal to. (Positive Integer)
! - m: The number of integers. (Positive Integer)
! - result: An array to store the M integers that meet the criteria.
! - size: The size of the result array.

! Example:
! - For N=6 and M=3, the output should be [2, 2, 2] since 2*2*2 is the maximum product
!   possible with three numbers summing up to 6, and it's also lexicographically the smallest combination.
  subroutine max_product_distribution(n, m, result, size)
    implicit none
    integer, intent(in) :: n, m, size
    integer, intent(out) :: result(size)
    integer :: k, r, i
    
    ! Check if m is greater than n, which is impossible for positive integers
    if (m > n) then
        do i = 1, size
            result(i) = 0
        end do
        return
    end if
    
    ! Calculate the base value and remainder
    k = n / m
    r = mod(n, m)
    
    ! Fill the result array
    do i = 1, m
        if (i <= r) then
            result(i) = k + 1
        else
            result(i) = k
        end if
    end do
    
    ! Sort the array in non-decreasing order to achieve lexicographically smallest order
    call sort_array(result, m)
    
contains

    subroutine sort_array(arr, arr_size)
        integer, intent(inout) :: arr(arr_size)
        integer, intent(in) :: arr_size
        integer :: i, j, temp
        
        do i = 1, arr_size - 1
            do j = i + 1, arr_size
                if (arr(i) > arr(j)) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                end if
            end do
        end do
    end subroutine sort_array
end program main