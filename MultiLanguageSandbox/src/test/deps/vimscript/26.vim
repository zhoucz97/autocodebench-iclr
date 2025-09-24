
" Classify the integer x based on the following method:
" If x is a single-digit number, x belongs to its own class.
" Otherwise, sum the digits of x, get a new x, and iterate until the
" class is determined.
" Parameters:
" - x (string): The integer to be classified as a string.
"   Returns:
"   int: The class to which the integer x belongs.
" Example:
" >>> echo ClassifyInteger('24')
" 6

function ClassifyInteger(x) abort
    " Convert the input string to a number
    let num = str2nr(a:x)
    
    " Continue summing digits until we get a single-digit number
    while num >= 10
        let sum = 0
        " Convert number to string to process each digit
        let num_str = string(num)
        for digit in split(num_str, '\zs')
            let sum += str2nr(digit)
        endfor
        let num = sum
    endwhile
    
    return num
endfunction

function! TestClassifyInteger() abort
    if assert_equal(ClassifyInteger('24'), 6) | cq | endif
    if assert_equal(ClassifyInteger('39'), 3) | cq | endif
    if assert_equal(ClassifyInteger('123456789'), 9) | cq | endif
    if assert_equal(ClassifyInteger('123456789012345678901234567890'), 9) | cq | endif
    if assert_equal(ClassifyInteger('12345'), 6) | cq | endif
    if assert_equal(ClassifyInteger('999999999'), 9) | cq | endif
endfunction

call TestClassifyInteger()
exit(0)