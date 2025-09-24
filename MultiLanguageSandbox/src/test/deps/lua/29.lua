-- No imports required for Lua
--[[
Evaluate the grade for an input integer.

Parameters:
- score (integer): The integer score to be evaluated.

Returns:
​​    char: The grade corresponding to the input score.
​​         If the score is between 90 and 100 (inclusive), returns 'A'.
​​         Otherwise, returns 'B'.

Example usage:
    print(evaluate_integer_grade(90)) -- Output: A
--]]

function evaluate_integer_grade(score)
    if score >= 90 and score <= 100 then
        return 'A'
    else
        return 'B'
    end
end

-- Example usage:
print(evaluate_integer_grade(90))  -- Output: A
print(evaluate_integer_grade(89))  -- Output: B
print(evaluate_integer_grade(100)) -- Output: A
function test_evaluate_integer_grade()
    assert(evaluate_integer_grade(90) == 'A') -- Corrected the assertion to compare against character literals
    assert(evaluate_integer_grade(89) == 'B') -- Additional test for a score less than 90
    assert(evaluate_integer_grade(95) == 'A') -- Additional test within the 'A' range
    assert(evaluate_integer_grade(100) == 'A') -- Additional test for the boundary condition
    assert(evaluate_integer_grade(101) == 'B') -- Additional test for a score greater than 100

    -- Print "All tests passed" if all assertions hold true (optional)
    -- print("All tests passed")
end

test_evaluate_integer_grade()