program main
  implicit none
  integer :: result

  ! Test Case 1
  result = remaining_apples(50, 10, 200)
  call assert(result == 30)

  ! Test Case 2
  result = remaining_apples(10, 5, 25)
  call assert(result == 5)

  ! Test Case 3
  result = remaining_apples(100, 1, 100)
  call assert(result == 0)

  ! Test Case 4 - Edge case where time to eat an apple is 0
  result = remaining_apples(50, 0, 100)
  call assert(result == 0)

  ! Test Case 5 - Edge case where no time has passed
  result = remaining_apples(50, 10, 0)
  call assert(result == 50)


  result = remaining_apples(50, 10, 3)
  ! print *, result
  call assert(result == 49)

contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Calculates the number of whole apples remaining after eating apples for a certain amount of time.
! Each apple takes a specific amount of time to eat, and apples are eaten one after another without pause.

! Arguments:
! - m: The total number of apples (1 <= m <= 100).
! - t: The time in minutes it takes to eat one apple (0 <= t <= 100).
! - s: The time in minutes that has passed (1 <= s <= 10000).

! Returns:
! - The number of whole apples remaining.

! Example:
! - If you start with 50 apples, each taking 10 minutes to eat, and 200 minutes have passed, 
!   then the function should return 30, indicating that 30 apples remain.

  function remaining_apples(m, t, s) result(remaining)
    implicit none
    integer, intent(in) :: m, t, s
    integer :: remaining
    integer :: apples_eaten

    ! Calculate the number of apples eaten
    if (t == 0) then
        ! If it takes 0 minutes to eat an apple, all apples are eaten immediately
        apples_eaten = m
    else
        ! Otherwise, calculate how many apples can be eaten in the given time
        apples_eaten = min(m, s / t)
    end if

    ! Calculate the remaining apples
    remaining = m - apples_eaten
end function remaining_apples

end program main