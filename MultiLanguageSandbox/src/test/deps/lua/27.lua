
--[[
    Transforms the case of a given letter.
    Parameters:
    - letter (string): The input letter to be transformed.
    Returns:
    - string: The letter with its case reversed. If the input is lowercase, it returns the uppercase version,
    and if the input is uppercase, it returns the lowercase version.

    Example usage:
    print(transform_letter_case('b')) -- Outputs: B
    print(transform_letter_case('Z')) -- Outputs: z
--]]

function transform_letter_case(letter)
    if letter:match("%l") then
        return letter:upper()
    elseif letter:match("%u") then
        return letter:lower()
    else
        return letter -- Return the letter as-is if it's not a letter (e.g., numbers, symbols)
    end
end

-- Example usage:
print(transform_letter_case('b')) -- Outputs: B
print(transform_letter_case('Z')) -- Outputs: z
function test_transform_letter_case()
    assert(transform_letter_case('b') == 'B')
    assert(transform_letter_case('B') == 'b')

    -- Additional test cases
    assert(transform_letter_case('z') == 'Z')
    assert(transform_letter_case('Z') == 'z')
    assert(transform_letter_case('m') == 'M')
    assert(transform_letter_case('M') == 'm')
    assert(transform_letter_case('1') == '1') -- Non-alphabetic, should be unchanged
    assert(transform_letter_case('!') == '!') -- Non-alphabetic, should be unchanged

    -- print("All tests passed successfully.")
end

test_transform_letter_case()