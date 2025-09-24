
--[[
Determine if two closed intervals intersect.
    Arguments:
    a, b: Representing the first closed interval [a, b] where 0 <= a <= b <= 1000.
    c, d: Representing the second closed interval [c, d] where 0 <= c <= d <= 1000.
    Returns:
    boolean: true if the intervals intersect, false otherwise.
]]

function are_intervals_intersecting(a, b, c, d)
    -- Check if the intervals intersect
    return not (b < c or d < a)
end
function test_are_intervals_intersecting()
    assert(are_intervals_intersecting(1, 1, 1, 2) == true)
    assert(are_intervals_intersecting(3, 5, 2, 6) == true)
    assert(are_intervals_intersecting(3, 5, 4, 7) == true)
    assert(are_intervals_intersecting(3, 5, 6, 7) == false)
    -- Additional test cases
    assert(are_intervals_intersecting(0, 0, 0, 0) == true)
    assert(are_intervals_intersecting(1, 3, 2, 4) == true)
    assert(are_intervals_intersecting(1, 3, 4, 6) == false)
    assert(are_intervals_intersecting(10, 20, 20, 30) == true)
    assert(are_intervals_intersecting(10, 20, 21, 30) == false)
    
    -- print("All tests passed.")
end

test_are_intervals_intersecting()