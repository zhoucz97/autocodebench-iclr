
--[[
You are given three digits a, b, c. Two of them are equal, but the third one is different from the other two.
Find the value that occurs exactly once.
Examples:
    extraNumber(0, 0, 1) returns 1
    extraNumber(4, 3, 4) returns 3
]]--

function extraNumber(a, b, c)
    if a == b then
        return c
    elseif a == c then
        return b
    else
        return a
    end
end
function testExtraNumber()
    assert(extraNumber(2, 7, 2) == 7)
    assert(extraNumber(3, 2, 2) == 3)
    assert(extraNumber(5, 5, 1) == 1)
    assert(extraNumber(500000000, 3, 500000000) == 3)
    assert(extraNumber(500000000, 500000000, 3) == 3)
end

testExtraNumber()