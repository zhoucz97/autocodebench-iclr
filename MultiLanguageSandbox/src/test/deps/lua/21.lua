
--[[
The task is to determine if it is possible to assemble the wooden squares from n buckets,
where each bucket contains a_i squares with a side length of 1, into a single larger square.
Input: length of list, array of numbers
    >>> Is_Square(1, {9})
    true
    >>> Is_Square(2, {14, 2})
    true
    >>> Is_Square(2, {7, 7})
    false
]]--

function Is_Square(length, squares)
    local total = 0
    for i = 1, length do
        total = total + squares[i]
    end
    local k = math.sqrt(total)
    return k == math.floor(k)
end
local function testIsSquare()
    assert(Is_Square(1, {9}) == true)
    assert(Is_Square(2, {14, 2}) == true)
    assert(Is_Square(2, {7, 7}) == false)
    assert(Is_Square(7, {1, 2, 3, 4, 5, 6, 7}) == false)
    assert(Is_Square(6, {1, 3, 5, 7, 9, 11}) == true)
    assert(Is_Square(4, {2, 2, 2, 2}) == false)

    -- Additional test cases
    assert(Is_Square(3, {4, 5, 6}) == false)
    assert(Is_Square(4, {16, 9, 4, 1}) == false)
    assert(Is_Square(5, {1, 1, 1, 1, 1}) == false)
    assert(Is_Square(2, {25, 25}) == false)
    assert(Is_Square(3, {10, 10, 5}) == true)

    -- print("All tests passed!")
end

testIsSquare()