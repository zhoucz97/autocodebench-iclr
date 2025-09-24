
--[[
Counts the number of different coloring methods for 'n' squares with 'm' colors,
considering the requirement that adjacent squares and the first/last squares
must have different colors.

Args:
- n (int): The number of squares.
- m (int): The number of colors.

Returns:
int: The count of different coloring methods satisfying the specified conditions.
Result is modulo 1000003.

Examples:
extraNumber(1, 1) => 1

--]]

function count_coloring_methods(n, m)
    if n == 1 then
        return 1 % 1000003
    end
    local result = ((m - 1)^n + (-1)^n * (m - 1)) % 1000003
    return result
end
function testCountColoringMethods()
    assert(count_coloring_methods(1, 1) == 1)
    assert(count_coloring_methods(2, 2) == 2)
    assert(count_coloring_methods(3, 3) == 6)
    assert(count_coloring_methods(4, 2) == 2)
    -- The following test is an example, as the expected result is not provided
    assert(count_coloring_methods(1000, 10) == 566585)
    
    -- Additional test cases
    assert(count_coloring_methods(2, 3) == 6)
    assert(count_coloring_methods(1000, 1000) == 67911)
    assert(count_coloring_methods(999, 66) == 501817)
    assert(count_coloring_methods(5, 3) == 30) -- Example of an expected output

    -- print("All tests passed!")
end

testCountColoringMethods()