
--[[
Classify the integer x based on the following method:
If x is a single-digit number, x belongs to its own class.
Otherwise, sum the digits of x, get a new x, and iterate until the class is determined.

Parameters:
- x (string): The integer to be classified as a string.
    
Returns:
- int: The class to which the integer x belongs.

Example usage:
classify_integer("24") --> 6
]]

function classify_integer(x)
    -- Convert the string to a number for digit operations
    local num = tonumber(x)
    
    -- Continue summing digits until we get a single-digit number
    while num >= 10 do
        local sum = 0
        -- Sum each digit of the number
        for digit in tostring(num):gmatch(".") do
            sum = sum + tonumber(digit)
        end
        num = sum
    end
    
    return num
end
function testClassifyInteger()
    assert(classify_integer("24") == 6)
    assert(classify_integer("39") == 3)
    assert(classify_integer("123456789") == 9)
    assert(classify_integer("123456789012345678901234567890") == 9)
    assert(classify_integer("12345") == 6)
    assert(classify_integer("999999999") == 9)
    -- print("All tests passed!")
end

testClassifyInteger()