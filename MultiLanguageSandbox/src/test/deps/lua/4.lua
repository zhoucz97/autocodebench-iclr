
--[[
Find the maximum and minimum of three distinct integers.

Parameters:
a (int): The first integer.
b (int): The second integer.
c (int): The third integer.

Returns:
A table with the maximum and minimum values, with "max" as the key for the maximum value and "min" as the key for the minimum value.

Example call:
local result = find_max_min(1,2,3)
assert(result.max == 3 and result.min == 1)
]]--

function find_max_min(a, b, c)
    -- Initialize max and min with the first value
    local max = a
    local min = a

    -- Compare with the second value
    if b > max then
        max = b
    elseif b < min then
        min = b
    end

    -- Compare with the third value
    if c > max then
        max = c
    elseif c < min then
        min = c
    end

    -- Return the result as a table
    return {max = max, min = min}
end
function testFindMaxMin()
    local result

    result = find_max_min(1, 2, 3)
    assert(result.max == 3 and result.min == 1)

    -- Additional tests
    result = find_max_min(5, 3, 4)
    assert(result.max == 5 and result.min == 3)

    result = find_max_min(10, -2, 7)
    assert(result.max == 10 and result.min == -2)

    result = find_max_min(-1, -3, -2)
    assert(result.max == -1 and result.min == -3)
end

testFindMaxMin()