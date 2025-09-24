program main
  implicit none

  integer, parameter :: n = 2, m = 3
  integer :: input_matrix(n, m), output_matrix(m, n), expected_output(m, n)
  integer :: i, j

  ! Initialize input matrix
  input_matrix = reshape([1, 2, 3, 4, 5, 6], shape(input_matrix))
  print *, input_matrix
  ! Expected output matrix
  expected_output = reshape([1, 3, 5, 2, 4, 6], shape(expected_output))

  ! Call the transpose subroutine
  call transpose_matrix(input_matrix, n, m, output_matrix)

  ! Testing
  do i = 1, m
      do j = 1, n
          call assert(output_matrix(i, j) == expected_output(i, j))
      end do
  end do

contains
subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Transposes a given n x m matrix.
!
! The subroutine 'transpose_matrix' takes a two-dimensional matrix 'input_matrix' of size n x m and returns its transpose, which is a m x n matrix. The transposed matrix is stored in 'output_matrix'.
! 
! Arguments:
! - input_matrix: An integer matrix of dimensions n (rows) by m (columns).
! - n: The number of rows in 'input_matrix'.
! - m: The number of columns in 'input_matrix'.
! - output_matrix: An integer matrix of dimensions m (rows) by n (columns) to store the transposed matrix.
!
! Example:
! Input matrix:
!  1  2
!  3  4
!  5  6
! Transposed matrix:
!  1  3  5
!  2  4  6

subroutine transpose_matrix(input_matrix, n, m, output_matrix)
    implicit none
    ! Declare variables
    integer, intent(in) :: n, m
    integer, dimension(n, m), intent(in) :: input_matrix
    integer, dimension(m, n), intent(out) :: output_matrix
    integer :: i, j
    
    ! Transpose the matrix
    do i = 1, n
        do j = 1, m
            output_matrix(j, i) = input_matrix(i, j)
        end do
    end do
end subroutine transpose_matrix
end program main