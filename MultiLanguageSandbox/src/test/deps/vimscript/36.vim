
" Given n doors arranged in a circle, the player starts in front of door 1. Each turn, the player 
" can choose a number i and pay a cost C_i to move i steps to the right and then open the door at
" that position. It is guaranteed that C_i >= C_{i+1} for 1 <= i < n. The task is to determine the 
" minimum total cost required to open all doors.
"
" Example:
" >>> MinTotalCost(3, [1, 1, 1])
" 3

function! TestMinTotalCost() abort
    if assert_equal(MinTotalCost(5, [4, 3, 3, 3, 3]), 15) | cq | endif
    if assert_equal(MinTotalCost(3, [1, 1, 1]), 3) | cq | endif
    if assert_equal(MinTotalCost(4, [5, 4, 3, 2]), 11) | cq | endif
    if assert_equal(MinTotalCost(4, [100, 99, 98, 97]), 391) | cq | endif
    if assert_equal(MinTotalCost(6, [10, 9, 8, 7, 6, 5]), 35) | cq | endif
    if assert_equal(MinTotalCost(7, [2, 2, 2, 2, 2, 2, 2]), 14) | cq | endif
    if assert_equal(MinTotalCost(8, [9, 7, 7, 7, 7, 7, 7, 7]), 56) | cq | endif
    if assert_equal(MinTotalCost(9, [3, 2, 2, 2, 2, 2, 2, 2, 2]), 18) | cq | endif
    if assert_equal(MinTotalCost(10, [6, 5, 5, 5, 5, 5, 5, 5, 5, 5]), 50) | cq | endif
    if assert_equal(MinTotalCost(11, [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]), 11) | cq | endif
endfunction

call TestMinTotalCost()
exit(0)