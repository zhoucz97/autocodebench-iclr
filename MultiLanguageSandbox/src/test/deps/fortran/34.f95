program main
  implicit none

  integer, dimension(7) :: arr
  integer, dimension(10) :: arr2, ex2
  integer :: i

  ! Initialize array
  arr = [5, 2, 6, 1, 7, 3, 4]

  ! Call quick_sort
  call quick_sort(arr, 1, size(arr))

  ! Expected sorted array: [1, 2, 3, 4, 5, 6, 7]
  ! Test
  do i = 1, size(arr)
      call assert(arr(i) == i)
  end do


  arr2 = [4, 3, 4, 3, 1, 2, 11, 21, 12, 11]
  ex2 = [1, 2, 3, 3, 4, 4, 11, 11, 12, 21]
  ! Call quick_sort
  call quick_sort(arr2, 1, size(arr2))

  ! Expected sorted array: [1, 2, 3, 3, 4, 4, 11, 11, 12, 21]
  ! Test

  do i = 1, size(arr2)
      call assert(arr2(i) == ex2(i))
  end do

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert
function partition(arr, left, right) result(pivot_index)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_index, i, j, pivot, temp
    
    pivot = arr(right)  ! Choose the last element as pivot
    i = left - 1        ! Index of smaller element
    
    do j = left, right - 1
        if (arr(j) <= pivot) then
            i = i + 1
            ! Swap arr(i) and arr(j)
            temp = arr(i)
            arr(i) = arr(j)
            arr(j) = temp
        end if
    end do
    
    ! Swap arr(i+1) and arr(right) (or pivot)
    temp = arr(i + 1)
    arr(i + 1) = arr(right)
    arr(right) = temp
    
    pivot_index = i + 1
end function partition
! This subroutine implements the Quick Sort algorithm in Fortran. Quick Sort is an efficient, in-place, comparison-based, divide and conquer sorting algorithm.

! Arguments:
! - arr: An integer array that needs to be sorted.
! - left: The starting index of the array/sub-array.
! - right: The ending index of the array/sub-array.

! Example:
! - Given an array arr = [5, 2, 6, 1, 7, 3, 4], calling quick_sort(arr, 1, size(arr)) will sort the array in ascending order.
recursive subroutine quick_sort(arr, left, right)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: pivot_index
    
    if (left < right) then
        ! Partition the array and get the pivot index
        pivot_index = partition(arr, left, right)
        
        ! Recursively sort the left and right partitions
        call quick_sort(arr, left, pivot_index - 1)
        call quick_sort(arr, pivot_index + 1, right)
    end if
end subroutine quick_sort
end program main