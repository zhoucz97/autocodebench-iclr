
" Counts the number of different coloring methods for n squares with m colors,
" considering the requirement that adjacent squares and the first/last squares
" must have different colors.
" Args:
" - n (int): The number of squares.
" - m (int): The number of colors.
" Returns:
" int: The count of different coloring methods satisfying the specified conditions.
" Result is modulo 1000003.
" Example:
" >>> count_coloring_methods(1,1)
"     1

function CountColoringMethods(n, m)
    if a:n == 1
        return a:m % 1000003
    endif
    let mod = 1000003
    let term1 = pow((a:m - 1), a:n, mod)
    let term2 = (-1) ** a:n * (a:m - 1)
    let total = (term1 + term2) % mod
    return total < 0 ? total + mod : total
endfunction

function! TestCountColoringMethods()
    if assert_equal(CountColoringMethods(1, 1), 1) | cq | endif
    if assert_equal(CountColoringMethods(2, 2), 2) | cq | endif
    if assert_equal(CountColoringMethods(3, 3), 6) | cq | endif
    if assert_equal(CountColoringMethods(4, 2), 2) | cq | endif
    if assert_equal(CountColoringMethods(1000, 10), 566585) | cq | endif
    " Additional test cases
    if assert_equal(CountColoringMethods(2, 3), 6) | cq | endif
    if assert_equal(CountColoringMethods(1000, 1000), 67911) | cq | endif
    if assert_equal(CountColoringMethods(999, 66), 501817) | cq | endif
    if assert_equal(CountColoringMethods(5, 3), 30) | cq | endif
endfunction

call TestCountColoringMethods()
exit(0)