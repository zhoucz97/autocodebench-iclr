
" Function to calculate the value of a mathematical function
" for a given input and round to 5 decimal places.
"
" Parameters:
" - x (int): Input value for the function.
"
" Returns:
" - string: If x is not in the defined domain, returns "Not define".
"            Otherwise, returns the calculated function value as a string.
"
" Function Definitions:
" - For 0 <= x < 10: y = cos(x + 3.0)
" - For 10 <= x < 20: y = (cos(x + 7.5))^2
" - For 20 <= x < 30: y = (cos(x + 4.0))^4
"
" Example:
"     >>> calculate_function_value(40)
"         Not define
"

function CalculateFunctionValue(x)
    if a:x >= 0 && a:x < 10
        let y = cos(a:x + 3.0)
    elseif a:x >= 10 && a:x < 20
        let y = pow(cos(a:x + 7.5), 2)
    elseif a:x >= 20 && a:x < 30
        let y = pow(cos(a:x + 4.0), 4)
    else
        return "Not defined"
    endif
    
    " Round to 5 decimal places and convert to string
    let rounded_y = printf('%.5f', y)
    return rounded_y
endfunction

function! TestCalculateFunctionValue()
    if assert_equal(CalculateFunctionValue(40), "Not define") | cq | endif
    " Additional test cases based on provided function definitions
    if assert_equal(CalculateFunctionValue(40), "Not define") | cq | endif
    if assert_equal(CalculateFunctionValue(5), "-0.14550") | cq | endif
    if assert_equal(CalculateFunctionValue(15), "0.76266") | cq | endif
    if assert_equal(CalculateFunctionValue(25), "0.31314") | cq | endif
    if assert_equal(CalculateFunctionValue(-1), "Not define") | cq | endif
endfunction

call TestCalculateFunctionValue()
exit(0)