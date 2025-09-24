program main
  implicit none
  character :: max_char
  integer :: max_count

  ! Test 1
  call most_frequent_character("abbccc", max_char, max_count)
  call assert(max_char == 'c' .and. max_count == 3)

  ! Test 2
  call most_frequent_character("adfadffasdf", max_char, max_count)
  call assert(max_char == 'f' .and. max_count == 4)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Determines the character that appears the most in a given string composed of lowercase English letters ('a' to 'z').
! In case of a tie, returns the character with the smallest ASCII value.
!
! Arguments:
! - input_string: A string consisting of lowercase English letters.
! - max_char: The character that appears the most.
! - max_count: The number of times max_char appears in the string.
!
! Examples:
! - Calling most_frequent_character("abbccc", max_char, max_count) will set max_char to 'c' and max_count to 3.
! - Calling most_frequent_character("adfadffasdf", max_char, max_count) will set max_char to 'f' and max_count to 4.
subroutine most_frequent_character(input_string, max_char, max_count)
    implicit none
    character(len=*), intent(in) :: input_string
    character, intent(out) :: max_char
    integer, intent(out) :: max_count
    
    integer :: i, count(26) = 0  ! Array to count occurrences of each letter (a-z)
    integer :: current_count
    
    ! Initialize counts for all letters to 0
    count = 0
    
    ! Count occurrences of each character in the string
    do i = 1, len(input_string)
        ! Convert character to index (a=1, b=2, ..., z=26)
        current_count = iachar(input_string(i:i)) - iachar('a') + 1
        if (current_count >= 1 .and. current_count <= 26) then
            count(current_count) = count(current_count) + 1
        end if
    end do
    
    ! Find the character with the maximum count
    max_count = 0
    max_char = 'a'
    
    do i = 1, 26
        if (count(i) > max_count) then
            max_count = count(i)
            max_char = achar(iachar('a') + i - 1)
        else if (count(i) == max_count) then
            ! In case of tie, choose the character with smaller ASCII value
            if (achar(iachar('a') + i - 1) < max_char) then
                max_char = achar(iachar('a') + i - 1)
            end if
        end if
    end do
    
end subroutine most_frequent_character
end program main