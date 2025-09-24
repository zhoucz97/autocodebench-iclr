program main
  implicit none

  character(len=5) :: original
  character(len=5) :: encrypted

  ! Test case 1
  original = "CHINA"
  call encrypt_message(original, encrypted)
  call assert(encrypted == "GLMRE")

  original = "ABCDE"

  call encrypt_message(original, encrypted)
  call assert(encrypted == "EFGHI")

  ! Test case 2
  original = "XYZAA"
  call encrypt_message(original, encrypted)
  call assert(encrypted == "BCDEE")

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Encrypts a given message by replacing each letter with the fourth letter following it in the alphabet.
! The encryption rule is: for each letter in the original message, use the fourth subsequent letter in the alphabet as its replacement.
! The function assumes that the message contains only uppercase letters.

! Arguments:
! - original: A string representing the original message to be encrypted.
! - encrypted: A string representing the encrypted message.

! Example:
! - Given the input "ABCD", the output should be "EFGH".
! - For the input "XYZA", the output should be "BCDE".
subroutine encrypt_message(original, encrypted)
    implicit none
    character(len=*), intent(in) :: original
    character(len=len(original)), intent(out) :: encrypted
    integer :: i, ascii_val
    
    do i = 1, len(original)
        ascii_val = ichar(original(i:i))
        
        ! Handle wrap-around from 'Z' to 'A'
        if (ascii_val + 4 > ichar('Z')) then
            ascii_val = ascii_val - 26
        end if
        
        encrypted(i:i) = char(ascii_val + 4)
    end do
end subroutine encrypt_message
end program main