program main
  implicit none
  integer, parameter :: size = 5
  integer, parameter :: size2 = 3
  integer :: matrix(size, size)
  integer :: matrix2(size2, size2)
  integer :: sum1, sum2

  ! Initialize matrix
  matrix = reshape([&
      1, 0, 0, 0, 2, &
      0, 1, 0, 2, 0, &
      0, 0, 1, 0, 0, &
      0, 2, 0, 1, 0, &
      2, 0, 0, 0, 1 &
  ], shape(matrix))


  
  ! Call function
  call sum_diagonals(matrix, size, sum1, sum2)

  ! Test cases
  call assert(sum1 .eq. 5)
  call assert(sum2 .eq. 9)


  matrix2 = reshape([&
      1, 2, 3, &
      4, 5, 6, &
      7, 8, 9 &
  ], shape(matrix2))
  ! Call function
  call sum_diagonals(matrix2, size2, sum1, sum2)

  ! Test cases
  call assert(sum1 .eq. 15)
  call assert(sum2 .eq. 15)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Calculates the sum of elements on the two main diagonals of a square matrix.
! The function handles a square matrix of size 'size' and returns the sums of its two diagonals. 
! The first sum is from the top-left to bottom-right diagonal, and the second sum is from the top-right to bottom-left diagonal.

! Arguments:
! - matrix: A 2D integer array representing a square matrix.
! - size: The size of the dimensions of the square matrix.

! Returns:
! - sum1: The sum of the elements on the diagonal from top-left to bottom-right.
! - sum2: The sum of the elements on the diagonal from top-right to bottom-left.

! Example:
! Given a 3x3 matrix:
! 1 2 3
! 4 5 6
! 7 8 9
! sum_diagonals(matrix, 3, sum1, sum2) will set sum1 to 15 (1+5+9) and sum2 to 15 (3+5+7).

subroutine sum_diagonals(matrix, size, sum1, sum2)
    implicit none
    integer, intent(in) :: size
    integer, dimension(size, size), intent(in) :: matrix
    integer, intent(out) :: sum1, sum2
    integer :: i
    
    ! Initialize sums to zero
    sum1 = 0
    sum2 = 0
    
    ! Calculate sum of main diagonal (top-left to bottom-right)
    do i = 1, size
        sum1 = sum1 + matrix(i, i)
    end do
    
    ! Calculate sum of anti-diagonal (top-right to bottom-left)
    do i = 1, size
        sum2 = sum2 + matrix(i, size - i + 1)
    end do
end subroutine sum_diagonals
end program main