program main
  implicit none
  integer, dimension(24) :: scores
  integer, dimension(10) :: result
  integer, dimension(10) :: expected 
  integer :: i


  ! Initialize the scores array
  scores = [54, 27, 87, 16, 63, 40, 22, 61, 6, 57, 70, 42, 11, 50, 13, 5, 56, 7, 8, 86, 56, 91, 68, 59]
  expected = [91, 87, 86, 70, 68, 63, 61, 59, 57, 56]

  ! Call the subroutine
  call top_ten_scores(scores, 24, result)
  ! do i = 1, 10
  !   print *, result(i)
  ! end do

  do i = 1, size(result)
    call assert(result(i) == expected(i))
  end do
  
contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Finds the top ten highest values from a given array of scores.
!
! This subroutine sorts an array of integers in descending order and selects the top ten values.
! It handles cases where there are fewer than ten scores by returning as many scores as available.
!
! Arguments:
! - scores: An array of integers representing the scores.
! - n: The number of elements in the scores array.
!
! Returns:
! - result: An array of the top ten scores in descending order.
!
! Example:
! - Given scores array [54, 27, 87, 16, 63, 40, 22, 61, 6, 57, 70, 42, 11, 50, 13, 5, 56, 7, 8, 86, 56, 91, 68, 59] and n=24,
!   top_ten_scores should return [91, 87, 86, 70, 68, 63, 61, 59, 57, 56]
subroutine top_ten_scores(scores, n, result)
    implicit none
    integer, intent(in) :: n
    integer, dimension(n), intent(in) :: scores
    integer, dimension(10), intent(out) :: result
    integer, dimension(n) :: temp_scores
    integer :: i, j, k, temp
    
    ! Copy the input array to a temporary array to avoid modifying the original
    temp_scores = scores
    
    ! Sort the temporary array in descending order using bubble sort
    do i = 1, n-1
        do j = 1, n-i
            if (temp_scores(j) < temp_scores(j+1)) then
                temp = temp_scores(j)
                temp_scores(j) = temp_scores(j+1)
                temp_scores(j+1) = temp
            end if
        end do
    end do
    
    ! Fill the result array with the top ten scores
    do k = 1, min(10, n)
        result(k) = temp_scores(k)
    end do
    
    ! If there are fewer than 10 scores, fill the remaining positions with 0 (or any other placeholder)
    do k = min(10, n) + 1, 10
        result(k) = 0  ! or you could leave them uninitialized if preferred
    end do
end subroutine top_ten_scores
end program main