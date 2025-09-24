program main
  implicit none
  call assert(calculate("3+5") == "8")
  call assert(calculate("10-3") == "7")
  call assert(calculate("10%3") == "1")
  call assert(calculate("10/0") == "Error")
  call assert(calculate("5*2") == "10")
  call assert(calculate("10-3") == "7")
  call assert(calculate("10a3") == "Error")
  call assert(calculate("9/3") == "3")
  call assert(calculate("123+456") == "579")
  call assert(calculate("789-32") == "757")
  call assert(calculate("27*8") == "216")
  call assert(calculate("144/12") == "12")

contains

subroutine assert(condition)
    logical, intent(in) :: condition
    if (.not. condition) then
      write(*, *) 'Assertion failed!'
      stop
    end if
  end subroutine assert

! This function performs basic arithmetic operations - addition, subtraction, multiplication, division, and modulo - on two integers.
! It takes a single string argument representing the arithmetic expression, which must be in the format: "operand1 operator operand2".
! Valid operators are '+', '-', '*', '/', and '%'. The operands should be integers.

! Arguments:
! - expression: A string containing the arithmetic expression.

! Returns:
! - The result of the arithmetic operation as a string. If the operator is invalid or division by zero occurs, it returns "Error".

! Example:
! - calculate("3+5") returns "8"
! - calculate("10%3") returns "1"
! - calculate("10/0") returns "Error"
! - calculate("10a3") returns "Error"
function calculate(expression) result(output)
    implicit none
    character(len=*), intent(in) :: expression
    character(len=20) :: output
    integer :: i, op_pos, operand1, operand2, result_int
    character :: op
    
    ! Initialize output to "Error" in case of any issues
    output = "Error"
    
    ! Find the position of the operator
    op_pos = 0
    do i = 1, len_trim(expression)
        if (expression(i:i) == '+' .or. expression(i:i) == '-' .or. &
            expression(i:i) == '*' .or. expression(i:i) == '/' .or. &
            expression(i:i) == '%') then
            op_pos = i
            op = expression(i:i)
            exit
        end if
    end do
    
    ! Check if operator was found
    if (op_pos == 0) return
    
    ! Extract operands
    read(expression(1:op_pos-1), *, iostat=i) operand1
    if (i /= 0) return
    
    read(expression(op_pos+1:), *, iostat=i) operand2
    if (i /= 0) return
    
    ! Perform the operation
    select case (op)
        case ('+')
            result_int = operand1 + operand2
        case ('-')
            result_int = operand1 - operand2
        case ('*')
            result_int = operand1 * operand2
        case ('/')
            if (operand2 == 0) return
            result_int = operand1 / operand2
        case ('%')
            if (operand2 == 0) return
            result_int = mod(operand1, operand2)
        case default
            return
    end select
    
    ! Convert result to string
    write(output, '(I0)') result_int
end function calculate

end program main