program main
  implicit none
  character(len=100) :: test_string
  character(len=:), allocatable :: result_string

  ! Test case 1
  test_string = "happy new year 2019"
  result_string = remove_spaces(test_string)
  call assert(result_string == "happynewyear2019")

  ! Test case 2
  test_string = "Fortran 90/95"
  result_string = remove_spaces(test_string)
  call assert(result_string == "Fortran90/95")

  ! Test case 3
  test_string = " No Leading or Trailing Spaces "
  result_string = remove_spaces(test_string)
  call assert(result_string == "NoLeadingorTrailingSpaces")

  ! Test case 4
  test_string = " "
  result_string = remove_spaces(test_string)
  call assert(result_string == "")

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Removes all spaces from the provided input string and returns the modified string without spaces.
!
! Arguments:
! - input_string: A character string from which spaces are to be removed.
!
! Returns:
! - A new string with all spaces removed from the input string.
!
! Example:
! - Calling remove_spaces("happy new year 2019") will return "happynewyear2019".
function remove_spaces(input_string) result(output_string)
    implicit none
    character(len=*), intent(in) :: input_string
    character(len=len(input_string)) :: output_string
    integer :: i, j, n
    
    ! Initialize variables
    j = 1
    n = len(input_string)
    
    ! Loop through each character in the input string
    do i = 1, n
        if (input_string(i:i) /= ' ') then
            output_string(j:j) = input_string(i:i)
            j = j + 1
        end if
    end do
    
    ! Null-terminate the output string (optional, depending on Fortran version)
    if (j <= n) then
        output_string(j:) = ' '
    end if
end function remove_spaces

end program main