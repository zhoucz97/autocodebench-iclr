program main
  implicit none
  character(len=100) :: test_str, reversed_str

  test_str = "I am a student"
  call reverse_string(test_str, reversed_str)
  call  assert(reversed_str == "tneduts a ma I")

  test_str = "A string variable to store the reversed string, with a maximum length of 100 characters."
  call reverse_string(test_str, reversed_str)
  ! print *, reversed_str
  call  assert(reversed_str == ".sretcarahc 001 fo htgnel mumixam a htiw ,gnirts desrever eht erots ot elbairav gnirts A")

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Reverses the contents of the given string 'input_str' and stores the result in 'output_str'.
! The 'input_str' is a string whose length does not exceed 100 characters.
! The reversed string is stored in 'output_str'.

! Arguments:
! - input_str: A string to be reversed.
! - output_str: A string variable to store the reversed string, with a maximum length of 100 characters.

! Example:
! - Given input_str = "I am a student", the output_str will be "tneduts a ma I".

subroutine reverse_string(input_str, output_str)
    implicit none
    character(len=*), intent(in) :: input_str
    character(len=100), intent(out) :: output_str
    integer :: i, str_len
    
    ! Get the length of the input string
    str_len = len_trim(input_str)
    
    ! Initialize output string
    output_str = ''
    
    ! Reverse the string
    do i = 1, str_len
        output_str(i:i) = input_str(str_len - i + 1:str_len - i + 1)
    end do
    
end subroutine reverse_string
end program main