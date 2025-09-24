
--[[
Given two integers a and b, return the sum if the sum is even, or return the product of a and b if the sum is odd.

Examples:
    evenSumOrOddProduct(2, 3) -- returns 6
]]--

function evenSumOrOddProduct(a, b)
    local sum = a + b
    if sum % 2 == 0 then
        return sum
    else
        return a * b
    end
end
function testEvenSumOrOddProduct()
    assert(evenSumOrOddProduct(2, 3) == 6)
    assert(evenSumOrOddProduct(5, 5) == 10)
    assert(evenSumOrOddProduct(1, 1) == 2)
    assert(evenSumOrOddProduct(0, 0) == 0)
    assert(evenSumOrOddProduct(-1, -1) == -2)
    assert(evenSumOrOddProduct(100, 200) == 300)
    assert(evenSumOrOddProduct(3, 4) == 12)
    assert(evenSumOrOddProduct(-5, 5) == 0)
    assert(evenSumOrOddProduct(7, 8) == 56)
    assert(evenSumOrOddProduct(9, 10) == 90)
    assert(evenSumOrOddProduct(11, 14) == 154)
end

testEvenSumOrOddProduct()