program main
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  implicit none

  character(len=:), allocatable :: result

  result = remove_middle_stars("****a*bc*def*g****")
  call assert(trim(result) == "****abcdefg")

  result = remove_middle_stars("*****h*i*j***")
  call assert(trim(result) == "*****hij")

  result = remove_middle_stars("***x*y*z")
  call assert(trim(result) == "***xyz")

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! This function removes middle and trailing asterisks (*) from a string while preserving leading asterisks and alphabetic characters.
! Leading asterisks are defined as consecutive asterisks at the beginning of the string.

! Arguments:
! - input_str: A string containing letters and asterisks. The string must have leading asterisks, and asterisks between each letter.

! Returns:
! - output_str: The modified string with middle and trailing asterisks removed.

! Example:
! - remove_middle_stars("****a*bc*def*g****") returns "****abcdefg"
function remove_middle_stars(input_str) result(output_str)
    implicit none
    character(len=*), intent(in) :: input_str
    character(len=len(input_str)) :: output_str
    integer :: i, j, n, leading_asterisks

    ! Initialize variables
    n = len(input_str)
    j = 1
    leading_asterisks = 0

    ! Count leading asterisks
    do i = 1, n
        if (input_str(i:i) == '*') then
            leading_asterisks = leading_asterisks + 1
        else
            exit
        end if
    end do

    ! Copy leading asterisks to the output string
    output_str(1:leading_asterisks) = input_str(1:leading_asterisks)

    ! Process the rest of the string, removing middle and trailing asterisks
    do i = leading_asterisks + 1, n
        if (input_str(i:i) /= '*') then
            output_str(j:j) = input_str(i:i)
            j = j + 1
        end if
    end do

    ! Trim the output string to remove any extra spaces
    output_str = trim(output_str)
end function remove_middle_stars

end program main