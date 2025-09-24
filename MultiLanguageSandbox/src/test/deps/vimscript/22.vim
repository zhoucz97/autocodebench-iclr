
" Given integers c and d, where a + b = c and a * b = d, this function computes
" the possible value of a (a <= b). If there are multiple pairs (a, b) that satisfy
" the conditions, it outputs the pair with the smallest a. If such a pair does not
" exist, it returns -1.
" 
" Parameters:
"   - c (int): The sum of a and b.
"   - d (int): The product of a and b.
"
" Returns:
"   - int: A possible value of a, or -1 if no valid values are found.
"
" Examples:
"   >>> FindIntegers(7, 11)
"   -1

function FindIntegers(c, d)
    let discriminant = a:c * a:c - 4 * a:d
    if discriminant < 0
        return -1
    endif
    let sqrt_discriminant = float2nr(sqrt(discriminant))
    if sqrt_discriminant * sqrt_discriminant != discriminant
        return -1
    endif
    let a1 = (a:c - sqrt_discriminant) / 2
    let a2 = (a:c + sqrt_discriminant) / 2
    if a1 == a2
        return a1
    else
        return min([a1, a2])
    endif
endfunction

function! TestFindIntegers()
    if assert_equal(FindIntegers(5, 6), 2) | cq | endif
    if assert_equal(FindIntegers(6, 9), 3) | cq | endif
    if assert_equal(FindIntegers(7, 12), 3) | cq | endif
    if assert_equal(FindIntegers(7, 11), -1) | cq | endif
    if assert_equal(FindIntegers(9, 8), 1) | cq | endif
    if assert_equal(FindIntegers(10, 25), 5) | cq | endif
    if assert_equal(FindIntegers(10000, 8765), -1) | cq | endif
endfunction

call TestFindIntegers()
exit(0)