
" Given a sequence of N piles of stones numbered from 1 to N, with each pile containing
" a positive number of stones, determine the winner of a game played by Charlie and Dan.
" Charlie goes first, and they take turns moving a positive number of stones from the
" leftmost non-empty pile to the adjacent right pile. The player who can only move
" stones in the last pile loses. Assuming both players use the best possible strategy,
" find out who will win the game.
" Example:
" >>> gameWinner([1, 2, 2])
" 'Dan'

function GameWinner(piles)
    let total = 0
    let n = len(a:piles)
    if n == 0
        return 'Dan'
    endif
    for i in range(n - 1)
        let total += a:piles[i]
    endfor
    if total % 2 == 1
        return 'Charlie'
    else
        return 'Dan'
    endif
endfunction

function! TestGameWinner()
    if assert_equal(GameWinner([1, 2, 2]), 'Dan') | cq | endif
    if assert_equal(GameWinner([5, 5, 5, 5, 5]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([2, 1, 2]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([3, 3, 3, 3]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([1, 1]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([2, 1]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]), 'Dan') | cq | endif
    if assert_equal(GameWinner([2, 2, 2, 2, 2, 2, 2, 2, 2, 1]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([10, 10, 10, 10, 10, 10, 10, 10, 10, 10]), 'Charlie') | cq | endif
    if assert_equal(GameWinner([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 'Dan') | cq | endif
endfunction

call TestGameWinner()
exit(0)