program main
  implicit none
  integer :: result
    ! Test cases
  result = count_coin_change_ways(4)
  call assert(result == 5)

  result = count_coin_change_ways(5)
  call assert(result == 8)

  result = count_coin_change_ways(3)
  call assert(result == 3)

  result = count_coin_change_ways(2)
  call assert(result == 2)

  result = count_coin_change_ways(15)
  call assert(result == 987)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the number of ways to exchange a given amount of money using coins of 1 and 2 units.
! This function employs a recursive approach to determine all possible combinations.

! Arguments:
! - n: An integer representing the value of the money to be exchanged. It should be between 1 and 10.

! Returns:
! - The total number of different ways to exchange the given amount of money.

! Example:
! - count_coin_change_ways(4) returns 5, as there are 5 ways to exchange a 4-unit bill:
!   1) 1+1+1+1, 2) 2+1+1, 3) 1+2+1, 4) 1+1+2, 5) 2+2
recursive integer function count_coin_change_ways(n) result(ways)
    implicit none
    integer, intent(in) :: n

    ! Base cases
    if (n == 0) then
        ways = 1
    else if (n < 0) then
        ways = 0
    else
        ! Recursive case: sum of ways using a 1-unit coin and a 2-unit coin
        ways = count_coin_change_ways(n - 1) + count_coin_change_ways(n - 2)
    end if
end function count_coin_change_ways

end program main