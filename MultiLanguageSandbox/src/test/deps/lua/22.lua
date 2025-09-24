
--[[
Given integers c and d, where a + b = c and a * b = d, find and return the possible value of a (a <= b).
If there are multiple groups, output the group with the smallest a.
If valid values for a and b do not exist, the function will return -1.

Parameters:
- c (int): The sum of a and b.
- d (int): The product of a and b.

Returns:
Optional[int]: A possible value of a.

Usage:
    print(find_integers(7, 11))  -- Output: -1
]]--

function find_integers(c, d)
    local discriminant = c * c - 4 * d
    if discriminant < 0 then
        return -1
    end
    local sqrt_discriminant = math.sqrt(discriminant)
    if sqrt_discriminant ~= math.floor(sqrt_discriminant) then
        return -1
    end
    local a1 = (c - sqrt_discriminant) / 2
    local a2 = (c + sqrt_discriminant) / 2
    if a1 == math.floor(a1) and a2 == math.floor(a2) then
        if a1 <= a2 then
            return a1
        else
            return a2
        end
    else
        return -1
    end
end
function testFindIntegers()
    assert(find_integers(5, 6) == 2)
    assert(find_integers(6, 9) == 3)
    assert(find_integers(7, 12) == 3)
    assert(find_integers(7, 11) == -1)
    assert(find_integers(9, 8) == 1)
    assert(find_integers(10, 25) == 5)
    assert(find_integers(10000, 8765) == -1)
    
    -- print("All tests passed successfully.")
end

testFindIntegers()