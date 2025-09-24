program main
  implicit none

    integer :: chickens, rabbits
    logical :: solvable

    ! Test case 1: Solvable scenario
    call solve_chicken_rabbit(14, 32, chickens, rabbits, solvable)
    call assert(chickens == 12 .and. rabbits == 2 .and. solvable)

    ! Test case 2: Unsolvable scenario
    call solve_chicken_rabbit(10, 16, chickens, rabbits, solvable)
    call assert(.not. solvable)

    ! Additional Test case: Another solvable scenario
    call solve_chicken_rabbit(20, 56, chickens, rabbits, solvable)
    call assert(chickens == 12 .and. rabbits == 8 .and. solvable)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Solves the classic 'Chicken and Rabbit' problem.
! Given the total number of animals (chickens and rabbits) and the total number of legs, the subroutine calculates the number of chickens and rabbits.

! Arguments:
! - total_count: Total number of animals (chickens and rabbits combined).
! - total_legs: Total number of legs of all animals.
! - chickens: Number of chickens (output).
! - rabbits: Number of rabbits (output).
! - solvable: A logical flag indicating if the problem has a solution (output).

! Returns:
! - This subroutine does not return a value but updates the 'chickens', 'rabbits', and 'solvable' variables.

! Example:
! - For total_count=14 and total_legs=32, chickens=12 and rabbits=2.
! - For total_count=10 and total_legs=16, the output is 'No answer' (unsolvable).
subroutine solve_chicken_rabbit(total_count, total_legs, chickens, rabbits, solvable)
    implicit none
    integer, intent(in) :: total_count, total_legs
    integer, intent(out) :: chickens, rabbits
    logical, intent(out) :: solvable
    
    ! Local variables
    integer :: temp_chickens, temp_rabbits
    
    ! Initialize output variables
    chickens = 0
    rabbits = 0
    solvable = .false.
    
    ! Check for basic constraints
    if (total_count < 0 .or. total_legs < 0) then
        return
    end if
    
    ! The maximum possible chickens is total_count (all chickens)
    ! Each chicken has 2 legs, so total_legs must be >= 2*total_count to have any solution
    if (total_legs < 2 * total_count) then
        return
    end if
    
    ! The maximum possible rabbits is total_count (all rabbits)
    ! Each rabbit has 4 legs, so total_legs must be <= 4*total_count to have any solution
    if (total_legs > 4 * total_count) then
        return
    end if
    
    ! Now check if (total_legs - 2*total_count) is divisible by 2
    ! Because: total_legs = 2*chickens + 4*rabbits
    !          total_legs = 2*(chickens + 2*rabbits)
    !          So (total_legs - 2*total_count) must be even (since total_count = chickens + rabbits)
    if (mod(total_legs - 2 * total_count, 2) /= 0) then
        return
    end if
    
    ! Calculate the solution
    temp_rabbits = (total_legs - 2 * total_count) / 2
    temp_chickens = total_count - temp_rabbits
    
    ! Verify the solution is non-negative
    if (temp_chickens >= 0 .and. temp_rabbits >= 0) then
        chickens = temp_chickens
        rabbits = temp_rabbits
        solvable = .true.
    end if
    
end subroutine solve_chicken_rabbit
end program main