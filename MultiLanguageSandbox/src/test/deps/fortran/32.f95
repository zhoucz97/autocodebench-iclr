program main
  implicit none
  character(len=1000) :: test_word1, test_word2, test_word3
! Initialize test words
  test_word1 = "Knowledge"
  test_word2 = "Workhard"
  test_word3 = "AttiTude"
  
  ! Run tests
  call assert(word_score(test_word1) == 96)
  call assert(word_score(test_word2) == 98)
  call assert(word_score(test_word3) == 100)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the score of a given word based on the position of each letter in the alphabet.
! Each letter's score is its position in the alphabet (A/a = 1, B/b = 2, ..., Z/z = 26).
! The function sums these values to compute the total score of the word.

! Arguments:
! - word: A string containing only uppercase and lowercase English letters.

! Returns:
! - The total score of the word as an integer.

! Examples:
! - word_score("Knowledge") returns 96
! - word_score("Workhard") returns 98
! - word_score("AttiTude") returns 100
function word_score(word) result(score)
    implicit none
    character(len=*), intent(in) :: word
    integer :: score
    integer :: i, char_code

    score = 0

    do i = 1, len(word)
        char_code = ichar(word(i:i))
        ! Convert to lowercase to handle both uppercase and lowercase letters
        if (char_code >= ichar('A') .and. char_code <= ichar('Z')) then
            char_code = char_code - ichar('A') + 1
        else if (char_code >= ichar('a') .and. char_code <= ichar('z')) then
            char_code = char_code - ichar('a') + 1
        end if
        score = score + char_code
    end do
end function word_score

end program main