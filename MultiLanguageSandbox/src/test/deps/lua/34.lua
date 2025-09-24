
--[[
You are given an array a of n integers. You can perform at most one operation where you select three integers i, j, x (1 <= i <= j <= n) and replace all elements from a[i] to a[j] with x, at a cost of (j - i + 1).
Find the minimum cost to make all elements in the array equal.
    >>> makeEqualAgain(3, {1, 2, 1})
    1
    >>> makeEqualAgain(5, {5, 5, 1, 5, 5})
    1
--]]

function makeEqualAgain(n, a)
    -- Check if all elements are already the same
    local first = a[1]
    local allSame = true
    for i = 2, n do
        if a[i] ~= first then
            allSame = false
            break
        end
    end
    if allSame then
        return 0
    end
    
    -- Find the leftmost position where the element is not equal to the first element
    local left = 1
    while left <= n and a[left] == first do
        left = left + 1
    end
    
    -- Find the rightmost position where the element is not equal to the first element
    local right = n
    while right >= 1 and a[right] == first do
        right = right - 1
    end
    
    -- The minimal cost is the smaller of the left segment or the right segment
    local costLeft = left - 1
    local costRight = n - right
    
    return math.min(costLeft, costRight)
end
function testMakeEqualAgain()
    assert(makeEqualAgain(6, {1,2,3,4,5,1}) == 4)
    assert(makeEqualAgain(7, {1,1,1,1,1,1,1}) == 0)
    assert(makeEqualAgain(8, {8,8,8,1,2,8,8,8}) == 2)
    assert(makeEqualAgain(3, {1,2,3}) == 2)
    assert(makeEqualAgain(7, {4,3,2,7,1,1,3}) == 6)
    assert(makeEqualAgain(9, {9,9,2,9,2,5,5,5,3}) == 7)
    assert(makeEqualAgain(3, {1, 2, 1}) == 1)
    assert(makeEqualAgain(5, {5, 5, 1, 5, 5}) == 1)
    assert(makeEqualAgain(4, {1, 1, 1, 1}) == 0)
    assert(makeEqualAgain(6, {2, 2, 2, 3, 2, 2}) == 1)
    assert(makeEqualAgain(1, {1}) == 0)
    assert(makeEqualAgain(2, {1, 2}) == 1)
    assert(makeEqualAgain(4, {1, 2, 2, 1}) == 2)
    assert(makeEqualAgain(7, {4, 4, 4, 3, 3, 4, 4}) == 2)
    assert(makeEqualAgain(6, {5, 4, 4, 4, 5, 5}) == 3)
    assert(makeEqualAgain(7, {1, 2, 1, 2, 1, 2, 1}) == 5)
end

testMakeEqualAgain()