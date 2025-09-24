program main
  implicit none
  character(len=100000) :: test_hex
  character(len=300000) :: result_oct

  ! Test case 1
  test_hex = "39"
  result_oct = hex_to_oct(test_hex)
  call assert(trim(result_oct) == "71")

  ! Test case 2
  test_hex = "123ABC"
  result_oct = hex_to_oct(test_hex)
  call assert(trim(result_oct) == "4435274")

  ! Additional test cases
  test_hex = "1"
  result_oct = hex_to_oct(test_hex)
  call assert(trim(result_oct) == "1")

  test_hex = "A"
  result_oct = hex_to_oct(test_hex)
  call assert(trim(result_oct) == "12")

  test_hex = "FFFF"
  result_oct = hex_to_oct(test_hex)
  call assert(trim(result_oct) == "177777")

  print *, "All tests passed!"


contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Converts a hexadecimal string (base-16) to an octal string (base-8).
! The input hexadecimal string is composed of characters 0-9 and uppercase A-F.
! The output octal string should not have leading zeros.

! Arguments:
! - hex_string: A character string representing the hexadecimal number. 
!   The string consists of characters from 0-9 and A-F, with a maximum length of 100000.

! Returns:
! - oct_string: A character string representing the octal number corresponding to the input hexadecimal number.

! Example:
! - hex_to_oct("39") returns "71"
! - hex_to_oct("123ABC") returns "4435274"
function hex_to_oct(hex_string) result(oct_string)
    implicit none
    character(len=*), intent(in) :: hex_string
    character(len=:), allocatable :: oct_string
    integer :: i, j, k, decimal, temp, length, max_oct_length
    character(len=100000) :: temp_oct
    
    ! First convert hex to decimal
    decimal = 0
    do i = 1, len(hex_string)
        select case (hex_string(i:i))
            case ('0':'9')
                decimal = decimal * 16 + ichar(hex_string(i:i)) - ichar('0')
            case ('A':'F')
                decimal = decimal * 16 + ichar(hex_string(i:i)) - ichar('A') + 10
        end select
    end do
    
    ! Handle the special case of zero
    if (decimal == 0) then
        oct_string = "0"
        return
    end if
    
    ! Now convert decimal to octal
    temp = decimal
    j = 0
    do while (temp > 0)
        j = j + 1
        temp_oct(j:j) = achar(mod(temp, 8) + ichar('0'))
        temp = temp / 8
    end do
    
    ! The octal digits are in reverse order, so we need to reverse them
    oct_string = ''
    do k = j, 1, -1
        oct_string = oct_string // temp_oct(k:k)
    end do
    
end function hex_to_oct

end program main