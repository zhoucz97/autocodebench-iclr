program main
  implicit none
  integer :: shared
  integer, parameter :: n=5, m=5
  integer, dimension(n) :: a = [1, 2, 3, 4, 5]
  integer, dimension(m) :: b = [1, 3, 4, 5, 6]

  shared = count_shared_students(n, m, a, b)
  call assert(shared == 4)


  a = [1, 2, 3, 7, 5]
  b = [1, 3, 4, 5, 6]

  shared = count_shared_students(n, m, a, b)
  call assert(shared == 3)

  a = [1, 2, 3, 4, 5]
  b = [1, 3, 4, 5, 2]

  shared = count_shared_students(n, m, a, b)
  call assert(shared == 5)
contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Calculates the number of students who have enrolled in both course A and B.
! Each course has a unique set of student IDs, and the function counts how many IDs are common between the two sets.
!
! Arguments:
! - n: Number of students enrolled in course A.
! - m: Number of students enrolled in course B.
! - a(n): Array containing the IDs of students enrolled in course A.
! - b(m): Array containing the IDs of students enrolled in course B.
! Returns:
! - The number of students who are enrolled in both courses.
! Example:
! - Given n=5, m=5, a=[1, 2, 3, 4, 5], and b=[1, 3, 4, 5, 6], the function returns 4.
integer function count_shared_students(n, m, a, b) result(shared_count)
    implicit none
    integer, intent(in) :: n, m
    integer, dimension(n), intent(in) :: a
    integer, dimension(m), intent(in) :: b
    integer :: i, j, found
    
    shared_count = 0
    
    ! Iterate through each student in course A
    do i = 1, n
        found = 0
        
        ! Check if this student is in course B
        do j = 1, m
            if (a(i) == b(j)) then
                found = 1
                exit
            end if
        end do
        
        ! If found, increment the count
        if (found == 1) then
            shared_count = shared_count + 1
        end if
    end do
end function count_shared_students

end program main