
--[[
You are given a 1 by n pixel image, where each pixel has a color represented by an integer. You can perform an operation where you pick a color and change all connected pixels of the same color to the chosen color. Two pixels are connected if they are adjacent and have the same color. Find the minimum number of operations required to make all pixels the same color.

Note: For each color, there are at most 20 pixels of that color.

Examples:
    >>> minOperations(5, {1, 2, 3, 2, 1})
    2
]]--

function minOperations(n, pixelArray)
    if n == 0 then return 0 end
    
    -- Create a map to hold the connected components for each color
    local colorComponents = {}
    
    -- Iterate through the pixel array to identify connected components
    local currentColor = pixelArray[1]
    local start = 1
    for i = 2, n do
        if pixelArray[i] ~= currentColor then
            -- Record the component [start, i-1] for currentColor
            if not colorComponents[currentColor] then
                colorComponents[currentColor] = 0
            end
            colorComponents[currentColor] = colorComponents[currentColor] + 1
            currentColor = pixelArray[i]
            start = i
        end
    end
    -- Don't forget the last component
    if not colorComponents[currentColor] then
        colorComponents[currentColor] = 0
    end
    colorComponents[currentColor] = colorComponents[currentColor] + 1
    
    -- Find the color with the minimal number of components
    local minOps = math.huge
    for color, count in pairs(colorComponents) do
        if count < minOps then
            minOps = count
        end
    end
    
    return minOps
end
local function testMinOperations()
    assert(minOperations(5, {1, 2, 3, 2, 1}) == 2)
    assert(minOperations(4, {1, 1, 2, 2}) == 1)
    assert(minOperations(5, {1, 2, 1, 4, 2}) == 3)
    assert(minOperations(5, {5, 5, 5, 5, 5}) == 0)
    assert(minOperations(6, {1, 1, 1, 2, 2, 2}) == 1)
    assert(minOperations(7, {1, 3, 3, 3, 2, 2, 2}) == 2)
    assert(minOperations(8, {4, 4, 4, 4, 3, 3, 3, 3}) == 1)
    assert(minOperations(9, {1, 2, 3, 4, 5, 6, 7, 8, 9}) == 8)
    assert(minOperations(10, {1, 2, 1, 2, 1, 2, 1, 2, 1, 2}) == 5)
    assert(minOperations(3, {3, 3, 3}) == 0)
    assert(minOperations(4, {2, 1, 1, 2}) == 1)
end

testMinOperations()