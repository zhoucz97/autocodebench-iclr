
" Determine if two closed intervals intersect.
" Args:
" a, b: Representing the first closed interval [a, b] where 0 <= a <= b <= 1000.
" c, d: Representing the second closed interval [c, d] where 0 <= c <= d <= 1000.
" Returns:
" int: 1 if the intervals intersect, 0 otherwise.

function AreIntervalsIntersecting(a, b, c, d)
    " Check if the intervals [a,b] and [c,d] intersect
    if a:b < a:c || a:d < a:a
        return 0
    else
        return 1
    endif
endfunction

function! TestAreIntervalsIntersecting()
    if assert_equal(AreIntervalsIntersecting(1, 1, 1, 2), 1) | cq | endif
    if assert_equal(AreIntervalsIntersecting(3, 5, 2, 6), 1) | cq | endif
    if assert_equal(AreIntervalsIntersecting(3, 5, 4, 7), 1) | cq | endif
    if assert_equal(AreIntervalsIntersecting(3, 5, 6, 7), 0) | cq | endif
    " Additional test cases
    if assert_equal(AreIntervalsIntersecting(0, 0, 0, 0), 1) | cq | endif
    if assert_equal(AreIntervalsIntersecting(1, 3, 2, 4), 1) | cq | endif
    if assert_equal(AreIntervalsIntersecting(1, 3, 4, 6), 0) | cq | endif
    if assert_equal(AreIntervalsIntersecting(10, 20, 20, 30), 1) | cq | endif
    if assert_equal(AreIntervalsIntersecting(10, 20, 21, 30), 0) | cq | endif
endfunction

call TestAreIntervalsIntersecting()
exit(0)