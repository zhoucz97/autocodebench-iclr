
--[[
Checks if a three-character string has exactly two characters that are the same.

Args:
    s (string): The three-character string to be checked.

Returns:
    string: Returns 'Yes' if the input has exactly two equal characters, otherwise 'No'.
    Examples:
    check_two_equal_digits("112") --> "Yes"
    check_two_equal_digits("123") --> "No"
]]--

function check_two_equal_digits(s)
    -- Check if the first and second characters are the same
    if s:sub(1, 1) == s:sub(2, 2) and s:sub(1, 1) ~= s:sub(3, 3) then
        return "Yes"
    -- Check if the first and third characters are the same
    elseif s:sub(1, 1) == s:sub(3, 3) and s:sub(1, 1) ~= s:sub(2, 2) then
        return "Yes"
    -- Check if the second and third characters are the same
    elseif s:sub(2, 2) == s:sub(3, 3) and s:sub(2, 2) ~= s:sub(1, 1) then
        return "Yes"
    else
        return "No"
    end
end
function test_check_two_equal_digits()
    assert(check_two_equal_digits("112") == "Yes")
    assert(check_two_equal_digits("123") == "No")
    assert(check_two_equal_digits("232") == "Yes")
    assert(check_two_equal_digits("444") == "No") -- All three characters are equal, not two.
    assert(check_two_equal_digits("121") == "Yes")

    -- Additional test cases to cover more scenarios
    assert(check_two_equal_digits("787") == "Yes")
    assert(check_two_equal_digits("999") == "No") -- All three characters are equal
    assert(check_two_equal_digits("890") == "No")
    assert(check_two_equal_digits("556") == "Yes")
    assert(check_two_equal_digits("353") == "Yes")
    
    -- print("All tests passed successfully.")
end

test_check_two_equal_digits()