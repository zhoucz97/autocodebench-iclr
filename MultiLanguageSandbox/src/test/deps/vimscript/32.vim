
" Given a sequence of n events, each occurring with a periodicity of a_i years,
" the problem is to find the year Y_n when the n-th event occurs. The countdown
" for event i+1 only starts the year following the occurrence of event i.
" Example:
" >>> apocalypseYear(6, [3, 2, 4, 5, 9, 18])
"     36

function ApocalypseYear(n, signs)
    let sum = 0
    for sign in a:signs
        let sum += sign
    endfor
    return sum
endfunction

function! TestApocalypseYear()
    if assert_equal(ApocalypseYear(6, [3, 2, 4, 5, 9, 18]), 36) | cq | endif
    if assert_equal(ApocalypseYear(5, [1, 2, 3, 4, 5]), 5) | cq | endif
    if assert_equal(ApocalypseYear(5, [1, 1, 1, 1, 1]), 5) | cq | endif
    if assert_equal(ApocalypseYear(6, [50, 30, 711, 200, 503, 1006]), 2012) | cq | endif
    if assert_equal(ApocalypseYear(2, [1, 2]), 2) | cq | endif
    if assert_equal(ApocalypseYear(3, [3, 1, 2]), 6) | cq | endif
    if assert_equal(ApocalypseYear(3, [2, 3, 4]), 4) | cq | endif
    if assert_equal(ApocalypseYear(4, [1, 2, 3, 4]), 4) | cq | endif
    if assert_equal(ApocalypseYear(4, [5, 7, 11, 13]), 13) | cq | endif
    if assert_equal(ApocalypseYear(5, [2, 2, 2, 2, 2]), 10) | cq | endif
    if assert_equal(ApocalypseYear(3, [6, 10, 15]), 15) | cq | endif
    if assert_equal(ApocalypseYear(3, [4, 6, 14]), 14) | cq | endif
    if assert_equal(ApocalypseYear(4, [50, 30, 711, 200]), 800) | cq | endif
    if assert_equal(ApocalypseYear(6, [1, 1, 1, 1, 1, 1]), 6) | cq | endif
    if assert_equal(ApocalypseYear(2, [1000000, 999999]), 1999998) | cq | endif
endfunction

call TestApocalypseYear()
exit(0)