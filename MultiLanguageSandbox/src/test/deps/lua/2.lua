
--[[
Calculate the area of a triangle given its three sides.

Parameters:
- a (number): Length of side 'a'.
- b (number): Length of side 'b'.
- c (number): Length of side 'c'.

Returns:
- number: If the provided sides form a triangle, returns the calculated area 
          with 2 decimal places. Otherwise, returns -1 to indicate invalid input.

Examples:
    print(calculate_triangle_area(3, 5, 4)) -- Should print 6
    print(calculate_triangle_area(1, 1, 4)) -- Should print -1 for not a triangle
--]]

function calculate_triangle_area(a, b, c)
    -- Check if the sides can form a triangle (triangle inequality theorem)
    if a + b <= c or a + c <= b or b + c <= a then
        return -1
    end
    
    -- Calculate semi-perimeter
    local s = (a + b + c) / 2
    
    -- Calculate area using Heron's formula
    local area = math.sqrt(s * (s - a) * (s - b) * (s - c))
    
    -- Round to 2 decimal places
    return math.floor(area * 100 + 0.5) / 100
end

-- Test cases
print(calculate_triangle_area(3, 5, 4)) -- Should print 6
print(calculate_triangle_area(1, 1, 4)) -- Should print -1 for not a triangle
function testCalculateTriangleArea()
    -- Triangle with sides 3, 5, 4 should return area close to 6
    local area = calculate_triangle_area(3, 5, 4)
    assert(math.abs(area - 6) < 1e-6)

    -- Not a triangle with sides 1, 1, 4
    area = calculate_triangle_area(1, 1, 4)
    assert(area == -1)

    -- Triangle with sides 7, 24, 25 should return area close to 84
    area = calculate_triangle_area(7, 24, 25)
    assert(math.abs(area - 84) < 1e-6)

    -- Triangle with sides 10.5, 6.2, 7.3 should return a correct area
    area = calculate_triangle_area(10.5, 6.2, 7.3)
    -- Assuming the test is for an area close to an expected value due to precision
    assert(math.abs(area - 22.15) < 1e-2)

    -- print("All tests passed successfully.")
end

testCalculateTriangleArea()