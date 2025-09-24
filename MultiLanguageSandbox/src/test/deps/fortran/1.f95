program CloseElements
  implicit none

  integer, parameter :: max_size = 5
  real(4), dimension(max_size) :: a, b, c, d
  integer :: size_a, size_b, size_c, size_d

  ! Initialize arrays
  a = [1.0, 2.0, 3.9, 4.0, 5.0]
  b = [1.0, 2.0, 5.9, 4.0, 5.0]
  c = [1.0, 2.0, 3.0, 4.0, 5.0]
  d = [1.1, 2.2, 3.1, 4.1, 5.1]

  ! Get sizes of arrays
  size_a = size(a)
  size_b = size(b)
  size_c = size(c)
  size_d = size(d)

  ! Run tests
  call assert(has_close_elements(a, size_a, 0.3)  .eqv. .true.)
  call assert(has_close_elements(a, size_a, 0.05) .eqv. .false.)
  call assert(has_close_elements(b, size_b, 0.95) .eqv. .true.)
  call assert(has_close_elements(b, size_b, 0.8)  .eqv. .false.)
  call assert(has_close_elements(c, size_c, 2.0)  .eqv. .true.)
  call assert(has_close_elements(d, size_d, 1.0)  .eqv. .true.)
  call assert(has_close_elements(d, size_d, 0.5)  .eqv. .false.)

contains

subroutine assert(condition)
  logical, intent(in) :: condition
  if (.not. condition) then
    write(*, *) 'Assertion failed!'
    stop
  end if
end subroutine assert

! Check if in given list of numbers, any two numbers are closer to each other than the given threshold.
! >>> has_close_elements([1.0, 2.0, 3.0], 3,0.5)
! false
! >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 6, 0.3)
! true

  logical function has_close_elements(numbers, size, threshold)
    implicit none
    integer, intent(in) :: size
    real, dimension(size), intent(in) :: numbers
    real, intent(in) :: threshold
    integer :: i, j
    
    has_close_elements = .false.
    
    do i = 1, size - 1
        do j = i + 1, size
            if (abs(numbers(i) - numbers(j)) < threshold) then
                has_close_elements = .true.
                return
            end if
        end do
    end do
end function has_close_elements

end program main