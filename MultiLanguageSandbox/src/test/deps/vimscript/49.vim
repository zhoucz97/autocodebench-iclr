
" You are playing a game called Nim. In this game, you start with a pile of n stones, and you and your opponent take
" turns to remove 1 to 3 stones from the pile. The one who removes the last stone wins the game. Given the number of
" stones n, determine if you can win the game if both you and your opponent play optimally.
" Note: You always take the first turn.
"
" Here are some cases:
" >>> canWinNim(1)
" true

function CanWinNim(n)
    return a:n % 4 != 0
endfunction

function! TestCanWinNim()
    if assert_equal(CanWinNim(1), 1) | cq | endif
    if assert_equal(CanWinNim(2), 1) | cq | endif
    if assert_equal(CanWinNim(3), 1) | cq | endif
    if assert_equal(CanWinNim(4), 0) | cq | endif
    if assert_equal(CanWinNim(5), 1) | cq | endif
    if assert_equal(CanWinNim(6), 1) | cq | endif
    if assert_equal(CanWinNim(7), 1) | cq | endif
    if assert_equal(CanWinNim(8), 0) | cq | endif
    if assert_equal(CanWinNim(9), 1) | cq | endif
    if assert_equal(CanWinNim(10), 1) | cq | endif
    if assert_equal(CanWinNim(11), 1) | cq | endif
    if assert_equal(CanWinNim(12), 0) | cq | endif
    if assert_equal(CanWinNim(13), 1) | cq | endif
    if assert_equal(CanWinNim(14), 1) | cq | endif
    if assert_equal(CanWinNim(15), 1) | cq | endif
    if assert_equal(CanWinNim(16), 0) | cq | endif
    if assert_equal(CanWinNim(17), 1) | cq | endif
    if assert_equal(CanWinNim(18), 1) | cq | endif
    if assert_equal(CanWinNim(19), 1) | cq | endif
    if assert_equal(CanWinNim(20), 0) | cq | endif
endfunction

call TestCanWinNim()
exit(0)