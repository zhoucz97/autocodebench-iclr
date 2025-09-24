program main
  implicit none
  character(len=200) :: test_str
  integer :: len_str, letters, digits, spaces, others

  test_str = 'Hello 123!'
  len_str = len_trim(test_str)

  ! Call the subroutine
  call count_characters(test_str, len_str, letters, digits, spaces, others)
  call assert(letters == 5 .and. digits == 3 .and. spaces == 1 .and. others == 1)

  ! Initialize test string
  test_str = 'aklsjflj123 sadf918u324 asdf91u32oasdf/.'';123'
  len_str = len_trim(test_str)

  ! Call the subroutine
  call count_characters(test_str, len_str, letters, digits, spaces, others)
  call assert(letters == 23 .and. digits == 16 .and. spaces == 2 .and. others == 4)

contains
subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Counts the number of English letters, digits, spaces, and other characters in a given string.
!
! Arguments:
! - input_str: A string whose characters are to be counted.
! - len_str: The length of the input string.
!
! Output:
! - count_letters: The number of English letters in the string.
! - count_digits: The number of digits in the string.
! - count_spaces: The number of spaces in the string.
! - count_others: The number of other characters in the string.
!
! Example:
! - Given "Hello 123!", the subroutine should set count_letters to 5, count_digits to 3, count_spaces to 1, and count_others to 1.
subroutine count_characters(input_str, len_str, count_letters, count_digits, count_spaces, count_others)
    implicit none
    character(len=*), intent(in) :: input_str
    integer, intent(in) :: len_str
    integer, intent(out) :: count_letters, count_digits, count_spaces, count_others
    integer :: i, ichar_val
    
    ! Initialize counters
    count_letters = 0
    count_digits = 0
    count_spaces = 0
    count_others = 0
    
    ! Loop through each character in the string
    do i = 1, len_str
        ichar_val = ichar(input_str(i:i))
        
        ! Check for uppercase letters (A-Z)
        if (ichar_val >= 65 .and. ichar_val <= 90) then
            count_letters = count_letters + 1
        ! Check for lowercase letters (a-z)
        else if (ichar_val >= 97 .and. ichar_val <= 122) then
            count_letters = count_letters + 1
        ! Check for digits (0-9)
        else if (ichar_val >= 48 .and. ichar_val <= 57) then
            count_digits = count_digits + 1
        ! Check for space (ASCII 32)
        else if (ichar_val == 32) then
            count_spaces = count_spaces + 1
        ! All other characters
        else
            count_others = count_others + 1
        end if
    end do
end subroutine count_characters
end program main