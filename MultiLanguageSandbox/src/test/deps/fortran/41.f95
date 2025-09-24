program main
    implicit none
    integer, parameter :: n = 3
    integer :: scores(n,3)
    integer :: result

    ! Initialize scores
    scores = reshape([90, 85, 80,&
                        90, 95, 100, &
                        90, 90, 91], shape(scores))
    ! Expected result is 2
    result = count_matched_pairs(scores, n)
    call assert(result == 2)


    ! Initialize scores
    scores = reshape([100, 85, 60,&
                        90, 95, 100, &
                        120, 90, 91], shape(scores))
    result = count_matched_pairs(scores, n)
    call assert(result == 0)


contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! This function counts the number of pairs of students who are considered "closely matched".
! Two students are "closely matched" if the difference in each of their subject scores (Chinese, Mathematics, English)
! does not exceed 5 points and the difference in their total scores does not exceed 10 points.

! Arguments:
! - scores: An integer array of dimensions (n, 3) where n is the number of students.
!           Each row represents a student's scores in the three subjects.
! - n: The number of students.

! Returns:
! - The count of pairs of students who are considered "closely matched".

! Example:
! - Given scores = [(90, 90, 90), (85, 95, 90), (80, 100, 91)] and n = 3,
!   the function returns 2 because there are two pairs of "closely matched" students.

function count_matched_pairs(scores, n) result(matched_pairs)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: scores(n, 3)
    integer :: matched_pairs
    integer :: i, j, diff_chinese, diff_math, diff_english, diff_total
    
    matched_pairs = 0
    
    do i = 1, n - 1
        do j = i + 1, n
            ! Calculate differences in each subject
            diff_chinese = abs(scores(i, 1) - scores(j, 1))
            diff_math = abs(scores(i, 2) - scores(j, 2))
            diff_english = abs(scores(i, 3) - scores(j, 3))
            
            ! Calculate difference in total scores
            diff_total = abs(sum(scores(i, :)) - sum(scores(j, :)))
            
            ! Check if all conditions are met
            if (diff_chinese <= 5 .and. diff_math <= 5 .and. &
                diff_english <= 5 .and. diff_total <= 10) then
                matched_pairs = matched_pairs + 1
            end if
        end do
    end do
end function count_matched_pairs

end program main