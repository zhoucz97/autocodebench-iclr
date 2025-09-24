program main
  implicit none
  integer :: result

  ! Test 1
  result = count_title_characters("234")
  call assert(result == 3)

  ! Test 2
  result = count_title_characters("Ca 45")
  call assert(result == 4)

  ! Test 3
  result = count_title_characters(" A B ")

  call assert(result == 2)

  ! Test 4
  result = count_title_characters("123")
  call assert(result == 3)

  print *, "All tests passed successfully."
contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Calculates the number of characters in a composition's title, excluding spaces and newline characters.
! The title may contain uppercase and lowercase English letters, digits, spaces, and newline characters.
!
! Arguments:
! - title: A string representing the title of the composition.
!
! Returns:
! - An integer representing the count of characters in the title, excluding spaces and newline characters.
!
! Example:
! - count_title_characters("Ca 45") returns 4.
  function count_title_characters(title) result(count)
    implicit none
    character(len=*), intent(in) :: title
    integer :: count
    integer :: i, n

    ! Initialize the count to 0
    count = 0

    ! Get the length of the title
    n = len_trim(title)

    ! Loop through each character in the title
    do i = 1, n
        ! Check if the character is not a space or newline
        if (title(i:i) /= ' ' .and. title(i:i) /= char(10)) then
            count = count + 1
        end if
    end do
end function count_title_characters

end program main