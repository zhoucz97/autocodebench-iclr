program main
  implicit none
  integer :: result

  ! Test case 1
  result = total_smoked_cigarettes(4, 3)
  call assert(result == 5)

  ! Test case 2
  result = total_smoked_cigarettes(10, 3)
  call assert(result == 14)

  ! Additional test case
  result = total_smoked_cigarettes(20, 4)
  call assert(result == 26)

  print *, "All test cases passed."
contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! Calculates the total number of cigarettes Peter can smoke given a starting number of cigarettes and a conversion rate of butts to new cigarettes.
!
! Arguments:
! - n: The initial number of cigarettes Peter has.
! - k: The number of cigarette butts required to exchange for one new cigarette.
!
! Returns:
! - The total number of cigarettes Peter can smoke.
!
! Example:
! - If Peter starts with 4 cigarettes and needs 3 butts to get a new one, he can smoke a total of 5 cigarettes.
! - If Peter starts with 10 cigarettes and needs 3 butts to get a new one, he can smoke a total of 14 cigarettes.

  function total_smoked_cigarettes(n, k) result(total)
    implicit none
    integer, intent(in) :: n, k
    integer :: total, butts, new_cigs

    ! Initialize variables
    total = n
    butts = n

    ! Loop until no more exchanges are possible
    do while (butts >= k)
        ! Calculate the number of new cigarettes from butts
        new_cigs = butts / k
        ! Add the new cigarettes to the total
        total = total + new_cigs
        ! Update the number of butts (remaining butts after exchange + new butts from smoked cigarettes)
        butts = mod(butts, k) + new_cigs
    end do

end function total_smoked_cigarettes

end program main