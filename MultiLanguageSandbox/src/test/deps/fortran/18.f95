program main
  implicit none

  integer :: scores(5)
  character(len=1) :: expected_grades(5)
  character(len=1) :: grade
  integer :: i

  ! Test data
  scores = [56, 67, 100, 123, 85]
  expected_grades = ['E', 'D', 'A', 'X', 'B']

  ! Testing
  do i = 1, size(scores)
      grade = convert_score_to_grade(scores(i))
      call assert(grade == expected_grades(i))
  end do

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! This function converts a numeric score into a letter grade based on predefined criteria.
! The grading scale is as follows:
! 90-100 -> 'A', 80-89 -> 'B', 70-79 -> 'C', 60-69 -> 'D', 0-59 -> 'E'.
! If the score is outside the 0-100 range, the function returns an error message.

! Arguments:
! - score: An integer representing the score in the range 0 to 100.

! Returns:
! - A single character string representing the grade ('A', 'B', 'C', 'D', 'E'), 
!   or 'X' if the score is outside the valid range.

! Examples:
! - convert_score_to_grade(85) returns 'B'
! - convert_score_to_grade(101) returns 'X' (indicating an error)
function convert_score_to_grade(score) result(grade)
    implicit none
    integer, intent(in) :: score
    character(len=1) :: grade
    
    if (score < 0 .or. score > 100) then
        grade = 'X'
    else if (score >= 90) then
        grade = 'A'
    else if (score >= 80) then
        grade = 'B'
    else if (score >= 70) then
        grade = 'C'
    else if (score >= 60) then
        grade = 'D'
    else
        grade = 'E'
    end if
end function convert_score_to_grade

end program main