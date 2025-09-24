
--[[
    Find the factorial of N and take the modulo 10007 of the result.
​    
    Parameters:
    - N: An integer representing the input value (N <= 10000).
    
    Returns:
    - The result after taking the modulo 10007 of the output.

    Examples:
    assert(process_request(1) == 1)
    assert(process_request(5) == 10)
]]

function process_request(n)
    if n < 0 then
        return 0 -- Factorial is not defined for negative numbers
    end
    
    local result = 1
    for i = 1, n do
        result = (result * i) % 10007
    end
    return result
end

-- Test cases
assert(process_request(1) == 1)
assert(process_request(5) == 120)
local function test_process_request()
    assert(process_request(0) == 1) -- Added test for boundary condition
    assert(process_request(1) == 1)
    assert(process_request(2) == 2)
    assert(process_request(3) == 6)
    assert(process_request(4) == 24)
    assert(process_request(10) == 6266) -- 10! % 10007 = 3628800 % 10007 = 6266
    assert(process_request(10000) == 6991) -- Added a test for upper boundary condition
end

test_process_request()
-- No equivalent in Lua for printf statement to indicate test status, as assert will automatically throw an error when a test fails.