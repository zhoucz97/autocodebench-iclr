
" The task is to determine if it is possible to assemble the wooden squares from n buckets,
" where each bucket contains a_i squares with a side length of 1, into a single larger square.
" Input: length of list, array of numbers
" Example
"     >>> Is_Square(1, [9])
"     1



function Is_Square(length, squares)
    let total = 0
    for num in a:squares
        let total += num
    endfor
    let root = float2nr(sqrt(total))
    if root * root == total
        return 1
    else
        return 0
    endif
endfunction

function! TestIsSquare()
    if assert_equal(Is_Square(1, [9]), 1) | cq | endif
    if assert_equal(Is_Square(2, [14, 2]), 1) | cq | endif
    if assert_equal(Is_Square(2, [7, 7]), 0) | cq | endif
    if assert_equal(Is_Square(7, [1, 2, 3, 4, 5, 6, 7]), 0) | cq | endif
    if assert_equal(Is_Square(6, [1, 3, 5, 7, 9, 11]), 1) | cq | endif
    if assert_equal(Is_Square(4, [2, 2, 2, 2]), 0) | cq | endif
    
    " Additional test cases
    if assert_equal(Is_Square(3, [4, 5, 6]), 0) | cq | endif
    if assert_equal(Is_Square(4, [16, 9, 4, 1]), 0) | cq | endif
    if assert_equal(Is_Square(5, [1, 1, 1, 1, 1]), 0) | cq | endif
    if assert_equal(Is_Square(2, [25, 25]), 0) | cq | endif
    if assert_equal(Is_Square(3, [10, 10, 5]), 1) | cq | endif

endfunction

call TestIsSquare()
exit(0)