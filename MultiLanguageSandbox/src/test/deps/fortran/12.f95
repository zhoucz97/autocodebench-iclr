program main
  implicit none
  integer :: result

  result = strcmp_custom("apple", "banana")
  call assert(result == -1)

  result = strcmp_custom("cherry", "apple")
  call assert(result == 1)

  result = strcmp_custom("date", "date")
  call assert(result == 0)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Compares two strings lexicographically (like the standard strcmp function in C).
! The function returns 1 if the first string is greater than the second, -1 if it is less, and 0 if they are equal.

! Arguments:
! - str1: A character string.
! - str2: A character string.

! Returns:
! - An integer indicating the result of comparison:
! - 1 if str1 > str2,
! - -1 if str1 < str2,
! - 0 if str1 == str2.

! Example:
! - strcmp_custom("apple", "banana") returns -1
! - strcmp_custom("cherry", "apple") returns 1
! - strcmp_custom("date", "date") returns 0
function strcmp_custom(str1, str2) result(comparison_result)
    implicit none
    character(len=*), intent(in) :: str1, str2
    integer :: comparison_result
    integer :: i, min_len
    
    ! Initialize result to 0 (equal)
    comparison_result = 0
    
    ! Determine the minimum length of the two strings
    min_len = min(len(str1), len(str2))
    
    ! Compare characters one by one
    do i = 1, min_len
        if (ichar(str1(i:i)) > ichar(str2(i:i))) then
            comparison_result = 1
            return
        else if (ichar(str1(i:i)) < ichar(str2(i:i))) then
            comparison_result = -1
            return
        end if
    end do
    
    ! If all compared characters are equal, check string lengths
    if (len(str1) > len(str2)) then
        comparison_result = 1
    else if (len(str1) < len(str2)) then
        comparison_result = -1
    end if
end function strcmp_custom

end program main