
--[[
Count the number of odd integers in a given list of numbers.

Parameters:
- count (int): The count of numbers to analyze.
- ... (int): A sequence of integers.

Returns:
int: The count of odd numbers in the provided sequence.

Examples:
    count_odd_numbers(5, 1, 4, 3, 2, 5) -- returns 3
    count_odd_numbers(4, 2, 2, 0, 0) -- returns 0
    count_odd_numbers(6, 7, 7, 8, 1, 9, 10) -- returns 4
]]--

function count_odd_numbers(count, ...)
    local odd_count = 0
    local numbers = {...}  -- Convert the variable arguments into a table

    for i = 1, count do
        if numbers[i] % 2 ~= 0 then  -- Check if the number is odd
            odd_count = odd_count + 1
        end
    end

    return odd_count
end
function testCountOddNumbers()
    assert(count_odd_numbers(5, 1, 4, 3, 2, 5) == 3)
    assert(count_odd_numbers(4, 2, 2, 0, 0) == 0)
    assert(count_odd_numbers(6, 7, 7, 8, 1, 9, 10) == 4) -- Additional Test Sample

    -- print("All tests passed!")
end

testCountOddNumbers()