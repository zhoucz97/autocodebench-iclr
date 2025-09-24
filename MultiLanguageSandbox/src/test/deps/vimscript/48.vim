
" Given a non-negative integer num, repeatedly add all its digits until the result has only one digit.
" For example:
" >>> addDigits(38)
"     2
" Because 3 + 8 = 11, and 1 + 1 = 2. Since 2 has only one digit, 2 is the result.

function AddDigits(num)
    let n = a:num
    while n >= 10
        let sum = 0
        let str = string(n)
        for char in split(str, '\zs')
            let sum += str2nr(char)
        endfor
        let n = sum
    endwhile
    return n
endfunction

function! TestAddDigits()
    if assert_equal(AddDigits(38), 2) | cq | endif
    if assert_equal(AddDigits(0), 0) | cq | endif
    if assert_equal(AddDigits(9), 9) | cq | endif
    if assert_equal(AddDigits(123), 6) | cq | endif
    if assert_equal(AddDigits(456), 6) | cq | endif
    if assert_equal(AddDigits(9999), 9) | cq | endif
    if assert_equal(AddDigits(100), 1) | cq | endif
    if assert_equal(AddDigits(1010), 2) | cq | endif
    if assert_equal(AddDigits(1234), 1) | cq | endif
    if assert_equal(AddDigits(9876), 3) | cq | endif
    if assert_equal(AddDigits(199), 1) | cq | endif
endfunction

call TestAddDigits()
exit(0)