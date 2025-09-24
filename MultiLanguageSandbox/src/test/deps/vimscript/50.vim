
" Description:
"   Given two integers a and b, return the sum if the sum is even,
"   or return the product of a and b if the sum is odd.
" Examples:
"   >>> evenSumOrOddProduct(2, 3)
"   6
"   >>> evenSumOrOddProduct(5, 5)
"   10

function EvenSumOrOddProduct(a, b)
    let sum = a:a + a:b
    if sum % 2 == 0
        return sum
    else
        return a:a * a:b
    endif
endfunction

function! TestEvenSumOrOddProduct()
    if assert_equal(EvenSumOrOddProduct(2, 3), 6) | cq | endif
    if assert_equal(EvenSumOrOddProduct(5, 5), 10) | cq | endif
    if assert_equal(EvenSumOrOddProduct(1, 1), 2) | cq | endif
    if assert_equal(EvenSumOrOddProduct(0, 0), 0) | cq | endif
    if assert_equal(EvenSumOrOddProduct(-1, -1), -2) | cq | endif
    if assert_equal(EvenSumOrOddProduct(100, 200), 300) | cq | endif
    if assert_equal(EvenSumOrOddProduct(3, 4), 12) | cq | endif
    if assert_equal(EvenSumOrOddProduct(-5, 5), 0) | cq | endif
    if assert_equal(EvenSumOrOddProduct(7, 8), 56) | cq | endif
    if assert_equal(EvenSumOrOddProduct(9, 10), 90) | cq | endif
    if assert_equal(EvenSumOrOddProduct(11, 14), 154) | cq | endif
endfunction

call TestEvenSumOrOddProduct()
exit(0)