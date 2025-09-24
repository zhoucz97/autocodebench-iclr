program main
  implicit none
  integer, parameter :: n1 = 8, n2 = 4
  integer :: ids1(n1) = [5, 6, 8, 11, 9, 10, 12, 9]
  integer :: ids2(n2) = [1, 1, 2, 4]
  integer :: missing_id, duplicate_id

  ! Test Case 1
  call find_missing_and_duplicate_ids(ids1, n1, missing_id, duplicate_id)
  call assert(missing_id == 7 .and. duplicate_id == 9)

  ! Test Case 2
  call find_missing_and_duplicate_ids(ids2, n2, missing_id, duplicate_id)
  call assert(missing_id == 3 .and. duplicate_id == 1)

  print *, "All test cases passed."

contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! This subroutine finds the missing (discontinuous) and duplicate ID in a sequence of ticket IDs.
! It takes a list of IDs and the total number of IDs as input, and returns both the missing and duplicate ID.
!
! Arguments:
! - ids: An array of integers representing the ticket IDs.
! - n: The total number of ticket IDs.
!
! Returns:
! - missing_id: The ID that is missing in the sequence.
! - duplicate_id: The ID that appears more than once in the sequence.
!
! Example:
! Given IDs: [5, 6, 8, 11, 9, 10, 12, 9]
! The subroutine would return missing_id = 7 and duplicate_id = 9, as 7 is missing and 9 is duplicated.
subroutine find_missing_and_duplicate_ids(ids, n, missing_id, duplicate_id)
    implicit none
    integer, intent(in) :: n
    integer, dimension(n), intent(in) :: ids
    integer, intent(out) :: missing_id, duplicate_id
    
    integer :: i, j, count, min_id, max_id
    logical, dimension(:), allocatable :: present
    
    ! Find the minimum and maximum IDs in the array
    min_id = minval(ids)
    max_id = maxval(ids)
    
    ! Allocate and initialize the presence array
    allocate(present(min_id:max_id))
    present = .false.
    
    ! First pass: mark all present IDs and find duplicates
    duplicate_id = -1  ! Initialize to invalid value
    do i = 1, n
        if (present(ids(i))) then
            duplicate_id = ids(i)
        else
            present(ids(i)) = .true.
        end if
    end do
    
    ! Second pass: find the missing ID
    missing_id = -1  ! Initialize to invalid value
    do i = min_id, max_id
        if (.not. present(i)) then
            missing_id = i
            exit
        end if
    end do
    
    deallocate(present)
end subroutine find_missing_and_duplicate_ids
end program main