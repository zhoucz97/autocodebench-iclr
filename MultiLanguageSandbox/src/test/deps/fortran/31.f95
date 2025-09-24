program main
  implicit none

  character(len=100) :: str1, str2
  integer :: length1, length2, result

  ! Test case 1
  str1 = "abcdgh"
  str2 = "aedfhb"
  length1 = len_trim(str1)
  length2 = len_trim(str2)
  result = longest_common_subsequence_length(str1, str2, length1, length2)
  call assert(result == 3)

  ! Test case 2
  str1 = "aggtab"
  str2 = "gxtxayb"
  length1 = len_trim(str1)
  length2 = len_trim(str2)
  result = longest_common_subsequence_length(str1, str2, length1, length2)
  call assert(result == 4)

  ! Test case 3
  str1 = "cdsdsd"
  str2 = "aaaaa"
  length1 = len_trim(str1)
  length2 = len_trim(str2)
  result = longest_common_subsequence_length(str1, str2, length1, length2)
  call assert(result == 0)

  ! Test case 4
  str1 = "abcde"
  str2 = "abcde"
  length1 = len_trim(str1)
  length2 = len_trim(str2)
  result = longest_common_subsequence_length(str1, str2, length1, length2)
  call assert(result == 5)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Computes the length of the longest common subsequence (LCS) between two strings.
!
! This function determines the length of the longest subsequence present in both strings. 
! A subsequence is a sequence that appears in the same relative order, but not necessarily contiguous, 
! in both strings.
!
! Arguments:
! - str1: First string, consisting of lowercase English letters.
! - str2: Second string, consisting of lowercase English letters.
! - length1: The length of the first string.
! - length2: The length of the second string.
!
! Returns:
! - The length of the longest common subsequence between str1 and str2.
!
! Example:
! - longest_common_subsequence_length("abcdgh", "aedfhb", 6, 6) returns 3
function longest_common_subsequence_length(str1, str2, length1, length2) result(lcs_length)
    implicit none
    character(len=*), intent(in) :: str1, str2
    integer, intent(in) :: length1, length2
    integer :: lcs_length
    
    ! Local variables
    integer, dimension(0:length1, 0:length2) :: dp
    integer :: i, j
    
    ! Initialize DP table
    dp = 0
    
    ! Fill DP table
    do i = 1, length1
        do j = 1, length2
            if (str1(i:i) == str2(j:j)) then
                dp(i,j) = dp(i-1,j-1) + 1
            else
                dp(i,j) = max(dp(i-1,j), dp(i,j-1))
            end if
        end do
    end do
    
    ! The result is in the bottom-right corner of the table
    lcs_length = dp(length1, length2)
end function longest_common_subsequence_length

end program main