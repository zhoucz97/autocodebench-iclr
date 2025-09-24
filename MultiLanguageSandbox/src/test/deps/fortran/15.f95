program main
  implicit none
  integer :: result

  ! Test case 1
  result = extract_middle_bits(217)
  call assert(result == 13)

  ! Test case 2
  result = extract_middle_bits(255)  ! Binary: 11111111, Middle bits: 1111
  call assert(result == 15)

  ! Test case 3
  result = extract_middle_bits(34)   ! Binary: 100010, Middle bits: 0010
  call assert(result == 2)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Extracts the middle bits (4th to 7th from the right, start counting from 0) of a given decimal number's binary representation 
! and returns their decimal value.

! Arguments:
! - number: A positive integer in decimal format.

! Returns:
! - The decimal value of the 4th to 7th bits (from the right,start counting from 0) of the binary representation of 'number'.

! Example:
! - extract_middle_bits(217) returns 13, because the binary representation of 217 is 11011001, 
!   and the 4th to 7th bits from the right are 1101, which is 13 in decimal.
function extract_middle_bits(number) result(middle_bits)
    implicit none
    integer, intent(in) :: number
    integer :: middle_bits
    integer :: temp_number, bit_position, mask

    ! Initialize the result
    middle_bits = 0

    ! Create a mask to isolate the 4th to 7th bits (from the right)
    mask = b'1111'  ! Binary mask for the 4th to 7th bits

    ! Shift the number right by 3 positions to align the 4th to 7th bits with the least significant bits
    temp_number = ishft(number, -3)

    ! Apply the mask to extract the desired bits
    middle_bits = iand(temp_number, mask)

end function extract_middle_bits

end program main