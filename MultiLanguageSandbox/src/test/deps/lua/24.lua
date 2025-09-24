
--[[
Reads an integer and a character, then returns them as a formatted string separated by a comma.
Parameters:
- integer_value (number): The input integer.
- char_value (string): The input character.

Returns:
- string: A string containing the integer and character separated by a comma.

Example usage:
local result = process_integer_and_char(234,'H')
print(result) -- Output: 234,H
]]

function process_integer_and_char(integer_value, char_value)
    return tostring(integer_value) .. "," .. char_value
end
function testProcessIntegerAndChar()
    assert(process_integer_and_char(234, 'H') == "234,H")
    assert(process_integer_and_char(123, 'A') == "123,A")
    assert(process_integer_and_char(0, 'Z') == "0,Z")

    -- Additional tests can be added here with different input values.
end

-- Run the tests to verify that the function works as intended.
testProcessIntegerAndChar()