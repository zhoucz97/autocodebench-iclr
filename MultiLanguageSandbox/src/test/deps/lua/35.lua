
--[[
Given a sequence of N piles of stones numbered from 1 to N,
with each pile containing a positive number of stones, determine 
the winner of a game played by Charlie and Dan. Charlie goes first, 
and they take turns moving a positive number of stones from the 
leftmost non-empty pile to the adjacent right pile. The player who 
can only move stones in the last pile loses. Assuming both players 
use the best possible strategy, find out who will win the game.

    >>> gameWinner(3, {1, 2, 2})
    "Dan"
--]]

function gameWinner(n, piles)
    local total_stones = 0
    for _, stones in ipairs(piles) do
        total_stones = total_stones + stones
    end
    local total_moves = total_stones - n
    if total_moves % 2 == 1 then
        return "Charlie"
    else
        return "Dan"
    end
end
function testGameWinner()
    assert(gameWinner(3, {1, 2, 2}) == "Dan")
    assert(gameWinner(5, {5, 5, 5, 5, 5}) == "Charlie")
    assert(gameWinner(3, {2, 1, 2}) == "Charlie")
    assert(gameWinner(4, {3, 3, 3, 3}) == "Charlie")
    assert(gameWinner(2, {1, 1}) == "Charlie")
    assert(gameWinner(2, {2, 1}) == "Charlie")
    assert(gameWinner(11, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}) == "Dan")
    assert(gameWinner(10, {2, 2, 2, 2, 2, 2, 2, 2, 2, 1}) == "Charlie")
    assert(gameWinner(10, {10, 10, 10, 10, 10, 10, 10, 10, 10, 10}) == "Charlie")
    assert(gameWinner(10, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == "Dan")
end

testGameWinner()