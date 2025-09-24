program main
    implicit none
    integer :: result

    ! Test case 1
    result = sum_integers(1, 1)
    call assert(result == 2)

    ! Test case 2
    result = sum_integers(10, 20)
    call assert(result == 30)

    ! Test case 3: Exceeding range
    ! Uncomment to test exceeding range scenario
    result = sum_integers(2000, 3000)
    call assert(result == 5000)

  contains
  
  subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

  ! Calculates the sum of two integers A and B.
  ! 
  ! The function takes two integers as input and returns their sum. 
  ! 
  ! Arguments:
  ! - a: An integer.
  ! - b: An integer.
  !
  ! Returns:
  ! - The sum of a and b.
  !
  ! Example:
  ! - sum_integers(1, 1) returns 2
  ! - sum_integers(10, 20) returns 30
    function sum_integers(a, b) result(sum)
    implicit none
    integer, intent(in) :: a, b
    integer :: sum

    sum = a + b
end function sum_integers

end program main