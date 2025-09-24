
" Find the maximum and minimum of three distinct integers.
" Parameters:
" a (int): The first integer.
" b (int): The second integer.
" c (int): The third integer.
"
" Returns:
" Tuple[int, int]: This tuple concept is represented by returning a List in vimscript.
"
" >>> Example call:
" let [max, min] = FindMaxMin(1, 2, 3)
" assert_equal([3, 1], [max, min])

function FindMaxMin(a, b, c)
    " Initialize max and min with the first value
    let max = a:a
    let min = a:a

    " Compare with second value
    if a:b > max
        let max = a:b
    elseif a:b < min
        let min = a:b
    endif

    " Compare with third value
    if a:c > max
        let max = a:c
    elseif a:c < min
        let min = a:c
    endif

    " Return as a list (VimScript's closest equivalent to a tuple)
    return [max, min]
endfunction

function! TestFindMaxMin()
    if assert_equal(FindMaxMin(1, 2, 3), [3, 1]) | cq | endif
    if assert_equal(FindMaxMin(5, 3, 4), [5, 3]) | cq | endif
    if assert_equal(FindMaxMin(10, -2, 7), [10, -2]) | cq | endif
    if assert_equal(FindMaxMin(-1, -3, -2), [-1, -3]) | cq | endif
endfunction

call TestFindMaxMin()
exit(0)