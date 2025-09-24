program main
  implicit none
  integer :: h, m, s

  ! Test case 1: t = 5436
  call convert_seconds_to_hms(5436, h, m, s)
  call assert(h == 1 .and. m == 30 .and. s == 36)

  ! Test case 2: t = 3661
  call convert_seconds_to_hms(3661, h, m, s)
  call assert(h == 1 .and. m == 1 .and. s == 1)

  ! Test case 3: t = 0
  call convert_seconds_to_hms(0, h, m, s)
  call assert(h == 0 .and. m == 0 .and. s == 0) 

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! This subroutine converts a time value in seconds to hours, minutes, and seconds without leading zeros.
! The time is given as an integer and is then converted to hours, minutes, and seconds.
! The converted values are returned as separate integer values.

! Arguments:
! - t: An integer representing time in seconds (0 <= t <= 86399).

! Returns:
! - hours: The hour component of the time.
! - minutes: The minute component of the time.
! - seconds: The second component of the time.

! Example:
! - If t = 5436, then hours = 1, minutes = 30, seconds = 36.
subroutine convert_seconds_to_hms(t, hours, minutes, seconds)
    implicit none
    integer, intent(in) :: t
    integer, intent(out) :: hours, minutes, seconds
    
    ! Calculate hours, minutes, and seconds
    hours = t / 3600
    t = mod(t, 3600)  ! Remaining seconds after extracting hours
    minutes = t / 60
    seconds = mod(t, 60)
end subroutine convert_seconds_to_hms
end program main