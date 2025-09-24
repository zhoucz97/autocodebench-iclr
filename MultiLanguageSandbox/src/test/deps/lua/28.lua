
--[[
Display the ASCII information for a given character.
    
Parameters:
- character: The input character for which ASCII information is to be displayed.
    
Returns:
- The corresponding ASCII code

Example:
display_ascii_info('A') -- Returns 65
]]--

function display_ascii_info(character)
    -- Get the ASCII code of the character
    local ascii_code = string.byte(character)
    
    -- Display the ASCII information
            -- Return the ASCII code
    return ascii_code
end
function testDisplayAsciiInfo()
    assert(display_ascii_info('A') == 65)
    assert(display_ascii_info('B') == 66) -- Additional test case
    assert(display_ascii_info('0') == 48) -- Additional test case
    assert(display_ascii_info(' ') == 32) -- Additional test case
    -- print("All tests passed.")
end

testDisplayAsciiInfo()