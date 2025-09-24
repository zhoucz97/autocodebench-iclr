
--[[
Calculate the value of the function for a given input.
    Parameters:
      - x (integer): Input value for the function.
    Returns:
      - number or string: If x is not in the defined domain, the returned value is "Not define".
      Otherwise, the function returns the calculated value rounded to 5 decimal places.

    Function Definitions:
      - For 0 <= x < 10: y = cos(x + 3.0)
      - For 10 <= x < 20: y = (cos(x + 7.5))^2
      - For 20 <= x < 30: y = (cos(x + 4.0))^4
      Example Usage:
        print(calculate_function_value(5))  -- Expected Output: y represented as a string rounded to 5 decimal places
        print(calculate_function_value(40)) -- Expected Output: "Not define"
]]--

function calculate_function_value(x)
    if x >= 0 and x < 10 then
        local y = math.cos(x + 3.0)
        return string.format("%.5f", y)
    elseif x >= 10 and x < 20 then
        local y = math.cos(x + 7.5) ^ 2
        return string.format("%.5f", y)
    elseif x >= 20 and x < 30 then
        local y = math.cos(x + 4.0) ^ 4
        return string.format("%.5f", y)
    else
        return "Not define"
    end
end

-- Example usage:
print(calculate_function_value(5))   -- Expected Output: a string with the value rounded to 5 decimal places
print(calculate_function_value(40))  -- Expected Output: "Not define"
function test_calculate_function_value()
    assert(calculate_function_value(40) == "Not define")
    assert(calculate_function_value(5) == "-0.14550")
    assert(calculate_function_value(15) == "0.76266")
    assert(calculate_function_value(25) == "0.31314")
    assert(calculate_function_value(-1) == "Not define")
end

test_calculate_function_value()