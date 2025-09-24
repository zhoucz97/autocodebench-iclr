
" Given an integer n, this function computes the sum of all numbers from 1 to n
" that are multiples of either 3 or 5. Each number that satisfies this condition
" is counted only once, even if it is a multiple of both 3 and 5.
" Example:
" >>> call SumOfMultiples(10)
"     33 " (This includes 3, 5, 6, 9, and 10)

function SumOfMultiples(n)
    let sum = 0
    for i in range(1, a:n)
        if i % 3 == 0 || i % 5 == 0
            let sum += i
        endif
    endfor
    return sum
endfunction

function! TestSumOfMultiples()
    if assert_equal(SumOfMultiples(10), 33) | cq | endif
    if assert_equal(SumOfMultiples(15), 60) | cq | endif
    if assert_equal(SumOfMultiples(20), 98) | cq | endif
    if assert_equal(SumOfMultiples(5), 8) | cq | endif
    if assert_equal(SumOfMultiples(3), 3) | cq | endif
    if assert_equal(SumOfMultiples(6), 14) | cq | endif
    if assert_equal(SumOfMultiples(9), 23) | cq | endif
    if assert_equal(SumOfMultiples(12), 45) | cq | endif
    if assert_equal(SumOfMultiples(17), 60) | cq | endif
    if assert_equal(SumOfMultiples(21), 119) | cq | endif
    if assert_equal(SumOfMultiples(25), 168) | cq | endif
endfunction

call TestSumOfMultiples()
exit(0)