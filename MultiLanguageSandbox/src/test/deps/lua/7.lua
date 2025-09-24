
--[[
Calculates the area of a triangle given its base and height.

Parameters:
- base (number): The base length of the triangle.
- height (number): The height of the triangle.

Returns:
- number: The calculated area of the triangle, rounded to one decimal place.

Examples:
local area = calculate_triangle_area(1, 2)
print(area)  -- Output: 1.0

local area = calculate_triangle_area(3, 4)
print(area)  -- Output: 6.0
--]]

function calculate_triangle_area(base, height)
    local area = 0.5 * base * height
    return math.floor(area * 10 + 0.5) / 10
end
local function test_calculate_triangle_area()
    assert(math.abs(calculate_triangle_area(1, 2) - 1.0) < 1e-6)
    assert(math.abs(calculate_triangle_area(3, 4) - 6.0) < 1e-6)
    assert(math.abs(calculate_triangle_area(5, 8) - 20.0) < 1e-6)
    assert(math.abs(calculate_triangle_area(7, 3) - 10.5) < 1e-6)
    assert(math.abs(calculate_triangle_area(10, 10) - 50.0) < 1e-6)

    -- print("All tests passed.")
end

test_calculate_triangle_area()