program main
 
  implicit none
  integer :: result

  ! Test Case 1
  result = calculate_droid_earnings(5, 2)
  call assert(result == 730)

  ! Test Case 2
  result = calculate_droid_earnings(0, 10)
  call assert(result == -100)

  ! Additional Test Cases
  result = calculate_droid_earnings(10, 0)
  call assert(result == 1000)

  result = calculate_droid_earnings(3, 4)
  call assert(result == 110)
  
  print *, 'All tests passed successfully!'
contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Calculates the total earnings of Deliv-e-droid based on the number of successfully delivered and failed deliveries.
! Deliv-e-droid earns 50 units of money for each successful delivery and loses 10 units for each failed delivery.
! Additionally, if the number of successful deliveries is greater than the number of failed ones, Deliv-e-droid receives a bonus of 500 units.

! Arguments:
! - delivered: The number of successfully delivered packages (0 <= delivered <= 100).
! - failed: The number of failed deliveries (0 <= failed <= 100).

! Returns:
! - The total earnings of Deliv-e-droid, which can be negative if the losses outweigh the gains.

! Examples:
! - calculate_droid_earnings(5, 2) returns 730.
! - calculate_droid_earnings(0, 10) returns -100.

function calculate_droid_earnings(delivered, failed) result(earnings)
    implicit none
    integer, intent(in) :: delivered, failed
    integer :: earnings

    ! Calculate earnings from successful and failed deliveries
    earnings = delivered * 50 - failed * 10

    ! Add bonus if successful deliveries are greater than failed ones
    if (delivered > failed) then
        earnings = earnings + 500
    end if
end function calculate_droid_earnings

end program main