
--[[
Find the length of the longest consecutive sequence of 1s in the binary representation of a non-negative integer.
Parameters:
- n: A non-negative integer (0 ≤ n ≤ 2^64 - 1).

Returns:
- The length of the longest consecutive sequence of 1s in the binary representation of the given integer.

Examples:
extraNumber(7)    -- returns 3
]]--

function find_longest_consecutive_ones_length(n)
    local max_length = 0
    local current_length = 0
    
    while n > 0 do
        if n % 2 == 1 then
            current_length = current_length + 1
            if current_length > max_length then
                max_length = current_length
            end
        else
            current_length = 0
        end
        n = math.floor(n / 2)
    end
    
    return max_length
end
function testFindLongestConsecutiveOnesLength()
    assert(find_longest_consecutive_ones_length(7) == 3)
    assert(find_longest_consecutive_ones_length(13) == 2)
    assert(find_longest_consecutive_ones_length(12345) == 3)
    -- assert(find_longest_consecutive_ones_length(0xb111101111) == 4)
    assert(find_longest_consecutive_ones_length(0xFFFFFFFF) == 32)
    assert(find_longest_consecutive_ones_length(0) == 0)

    -- Uncomment the following line to print a success message
    -- print("All tests passed!")
end

testFindLongestConsecutiveOnesLength()