program main
  implicit none
  real(4) :: fahrenheit, celsius

  ! Test case 1
  fahrenheit = -40.0
  celsius = fahrenheit_to_celsius(fahrenheit)
  ! print *, 'Fahrenheit:', fahrenheit, '-> Celsius:', celsius  ! Expected output: -40.00
  call assert(celsius == -40.00)

  ! Test case 2
  fahrenheit = 32.0
  celsius = fahrenheit_to_celsius(fahrenheit)
  ! print *, 'Fahrenheit:', fahrenheit, '-> Celsius:', celsius  ! Expected output: 0.00

  call assert(celsius == 0.00)

  ! Test case 3
  fahrenheit = 212.0
  celsius = fahrenheit_to_celsius(fahrenheit)
  ! print *, 'Fahrenheit:', fahrenheit, '-> Celsius:', celsius  ! Expected output: 100.00
  call assert(celsius == 100.00)
  
contains
subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Converts a given Fahrenheit temperature to Celsius.
! The conversion formula used is C = 5 * (F - 32) / 9.
! The result is rounded to two decimal places.

! Arguments:
! - fahrenheit_temp: A real number representing the temperature in Fahrenheit.

! Returns:
! - celsius_temp: A real number representing the converted temperature in Celsius, rounded to two decimal places.

! Example:
! - fahrenheit_to_celsius(-40.0) returns -40.00
function fahrenheit_to_celsius(fahrenheit_temp) result(celsius_temp)
    implicit none
    real, intent(in) :: fahrenheit_temp
    real :: celsius_temp
    
    ! Convert Fahrenheit to Celsius using the formula C = 5 * (F - 32) / 9
    celsius_temp = 5.0 * (fahrenheit_temp - 32.0) / 9.0
    
    ! Round to two decimal places
    celsius_temp = nint(celsius_temp * 100.0) / 100.0
end function fahrenheit_to_celsius

end program main