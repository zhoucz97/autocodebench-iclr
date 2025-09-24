
--[[
Decode a series of numbers to reveal the pattern and understand the actual values
each digit represents.
0000=4 8888=8 1234=1 5678=3 9012=2 1357=0 2468=4

Parameters:
- data_str: A string representing a series of numbers.

Returns:
The result of each data string corresponding to the right-hand side of the equation.

Example usage:
assert(decode_numbers("0000") == 4);
]]--

function decode_numbers(data_str)
    local digit_values = {
        ['0'] = 1,
        ['1'] = 0,
        ['2'] = 0,
        ['3'] = 0,
        ['4'] = 1,
        ['5'] = 0,
        ['6'] = 1,
        ['7'] = 0,
        ['8'] = 2,
        ['9'] = 1
    }
    
    local sum = 0
    for i = 1, #data_str do
        local digit = data_str:sub(i, i)
        sum = sum + digit_values[digit]
    end
    return sum
end
function testDecodeNumbers()
    assert(decode_numbers("0000") == 4)
    assert(decode_numbers("8888") == 8)
    assert(decode_numbers("1234") == 1)
    assert(decode_numbers("5678") == 3)
    assert(decode_numbers("9012") == 2)
    assert(decode_numbers("1357") == 0)
    assert(decode_numbers("2468") == 4)
    -- Additional test samples
    assert(decode_numbers("9999") == 4)
    assert(decode_numbers("1111") == 0)
    assert(decode_numbers("2222") == 0)
    assert(decode_numbers("3333") == 0)
    assert(decode_numbers("4444") == 4)
    assert(decode_numbers("5555") == 0)
    assert(decode_numbers("6666") == 4)
    assert(decode_numbers("7777") == 0)
    assert(decode_numbers("0001") == 3)
    assert(decode_numbers("2301") == 1)
end

testDecodeNumbers()