
" You are given a 1 by n pixel image, where each pixel has a color represented by an integer. You can perform an operation 
" where you pick a color and change all connected pixels of the same color to the chosen color. Two pixels are connected if
" they are adjacent and have the same color. Find the minimum number of operations required to make all pixels the same color.
" 
" Note: For each color, there are at most 20 pixels of that color.
" 
" Examples:
"     >>> MinOperations(5, [1, 2, 3, 2, 1])
"     2

function! TestMinOperations()
    if assert_equal(MinOperations(5, [1, 2, 3, 2, 1]), 2) | cq | endif
    if assert_equal(MinOperations(4, [1, 1, 2, 2]), 1) | cq | endif
    if assert_equal(MinOperations(5, [1, 2, 1, 4, 2]), 3) | cq | endif
    if assert_equal(MinOperations(5, [5, 5, 5, 5, 5]), 0) | cq | endif
    if assert_equal(MinOperations(6, [1, 1, 1, 2, 2, 2]), 1) | cq | endif
    if assert_equal(MinOperations(7, [1, 3, 3, 3, 2, 2, 2]), 2) | cq | endif
    if assert_equal(MinOperations(8, [4, 4, 4, 4, 3, 3, 3, 3]), 1) | cq | endif
    if assert_equal(MinOperations(9, [1, 2, 3, 4, 5, 6, 7, 8, 9]), 8) | cq | endif
    if assert_equal(MinOperations(10, [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]), 5) | cq | endif
    if assert_equal(MinOperations(3, [3, 3, 3]), 0) | cq | endif
    if assert_equal(MinOperations(4, [2, 1, 1, 2]), 1) | cq | endif
endfunction

call TestMinOperations()
exit(0)