program main
  implicit none

  character(len=80) :: test_string
  integer :: result

  ! Test 1
  test_string = "this is a book"
  result = count_words_in_string(test_string)
  ! print*, "Test 1 (Expected 4): ", result
  call assert(result == 4)

  ! Test 2
  test_string = "hello world"
  result = count_words_in_string(test_string)
  ! print*, "Test 2 (Expected 2): ", result
  call assert(result == 2)

  ! Test 3
  test_string = "single"
  result = count_words_in_string(test_string)
  ! print*, "Test 3 (Expected 1): ", result
  call assert(result == 1)

  ! Test 4
  test_string = ""
  result = count_words_in_string(test_string)
  ! print*, "Test 4 (Expected 0): ", result
  call assert(result == 0)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Counts the number of words in a given string. A word is defined as a sequence of characters separated by spaces.
! This function assumes that words are separated by exactly one space and there are no leading or trailing spaces.

! Arguments:
! - input_string: A string with a maximum length of 80 characters, containing words separated by spaces.

! Returns:
! - word_count: The total number of words in the input string.

! Example:
! - count_words_in_string("this is a book") returns 4
function count_words_in_string(input_string) result(word_count)
    implicit none
    character(len=80), intent(in) :: input_string
    integer :: word_count
    integer :: i, n
    
    ! Initialize word count to 1 (at least one word exists)
    word_count = 1
    
    ! Get the length of the input string
    n = len_trim(input_string)
    
    ! Count the number of spaces (each space indicates a new word)
    do i = 1, n
        if (input_string(i:i) == ' ') then
            word_count = word_count + 1
        end if
    end do
end function count_words_in_string

end program main