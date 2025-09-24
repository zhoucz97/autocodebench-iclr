program main
  implicit none
  integer :: result

  ! Test cases
  result = day_of_year("1985/1/20")
  ! print *, result
  call assert(result == 20)

  result = day_of_year("2006/3/12")
  call assert(result == 71)

  result = day_of_year("2000/3/1")
  call assert(result == 61)  ! Leap year test

  result = day_of_year("2100/3/1")
  call assert(result == 60)  ! Non-leap year test (divisible by 100 but not 400)


contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Determines the day of the year for a given date in the format YYYY/MM/DD.

! Arguments:
! - date_string: A string representing a date in the format YYYY/MM/DD.

! Returns:
! - The day count as an integer, representing the day of the year.

! Example:
! - day_of_year("1985/1/20") returns 20
! - day_of_year("2006/3/12") returns 71
integer function day_of_year(date_string) result(day_count)
    implicit none
    character(len=10), intent(in) :: date_string
    integer :: day_count
    integer :: year, month, day
    integer :: i, days_in_month(12)
    logical :: is_leap
    
    ! Parse the input string
    read(date_string(1:4), '(I4)') year
    read(date_string(6:7), '(I2)') month
    read(date_string(9:10), '(I2)') day
    
    ! Days in each month (non-leap year)
    days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    
    ! Check for leap year
    is_leap = .false.
    if (mod(year, 4) == 0) then
        if (mod(year, 100) /= 0 .or. mod(year, 400) == 0) then
            is_leap = .true.
        end if
    end if
    
    ! Adjust February for leap years
    if (is_leap) then
        days_in_month(2) = 29
    end if
    
    ! Calculate day of year
    day_count = 0
    do i = 1, month - 1
        day_count = day_count + days_in_month(i)
    end do
    day_count = day_count + day
    
end function day_of_year

end program main