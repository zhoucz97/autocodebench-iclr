
--[[
You are playing a game called Nim. In this game, you start with a pile of n stones, and you and your opponent take turns to remove 1 to 3 stones from the pile. The one who removes the last stone wins the game. Given the number of stones n, determine if you can win the game assuming both you and your opponent play optimally.
Note: You always take the first turn.

Here are some cases:
    >>> canWinNim(1)
    true
]]--

function canWinNim(n)
    return n % 4 ~= 0
end
local function testCanWinNim()
    assert(canWinNim(1) == true)
    assert(canWinNim(2) == true)
    assert(canWinNim(3) == true)
    assert(canWinNim(4) == false)
    assert(canWinNim(5) == true)
    assert(canWinNim(6) == true)
    assert(canWinNim(7) == true)
    assert(canWinNim(8) == false)
    assert(canWinNim(9) == true)
    assert(canWinNim(10) == true)
    assert(canWinNim(11) == true)
    assert(canWinNim(12) == false)
    assert(canWinNim(13) == true)
    assert(canWinNim(14) == true)
    assert(canWinNim(15) == true)
    assert(canWinNim(16) == false)
    assert(canWinNim(17) == true)
    assert(canWinNim(18) == true)
    assert(canWinNim(19) == true)
    assert(canWinNim(20) == false)
end

testCanWinNim()