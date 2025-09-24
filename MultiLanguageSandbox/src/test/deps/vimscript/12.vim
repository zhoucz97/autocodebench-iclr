
" You are given three digits a, b, c. Two of them are equal, but the third one is different from
" the other two. Find the value that occurs exactly once.
" Examples:
" >>> ExtraNumber(0,0,1) returns 1
" >>> ExtraNumber(4,3,4) returns 3

function ExtraNumber(a, b, c)
    if a:a == a:b
        return a:c
    elseif a:a == a:c
        return a:b
    else
        return a:a
    endif
endfunction

function! TestExtraNumber()
    if assert_equal(ExtraNumber(2, 7, 2), 7) | cq | endif
    if assert_equal(ExtraNumber(3, 2, 2), 3) | cq | endif
    if assert_equal(ExtraNumber(5, 5, 1), 1) | cq | endif
    if assert_equal(ExtraNumber(500000000, 3, 500000000), 3) | cq | endif
    if assert_equal(ExtraNumber(500000000, 500000000, 3), 3) | cq | endif
endfunction

call TestExtraNumber()
exit(0)