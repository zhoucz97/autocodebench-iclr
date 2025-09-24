
--[[
Given two positive integers n and k, find the number of positive integers x, 
where 1 <= x <= n, that can be expressed as x = a^b with a and b being positive 
integers and b >= k. Different legal representations of the same number are 
counted only once.

    >>> countPowerNumbers(99, 1)
    99
]]--

function countPowerNumbers(n, k)
    local seen = {}
    local count = 0
    
    -- Iterate over possible bases a
    for a = 1, math.floor(n^(1/k)) + 1 do
        local b = k
        while true do
            local x = a^b
            if x > n then
                break
            end
            if not seen[x] then
                seen[x] = true
                count = count + 1
            end
            b = b + 1
        end
    end
    
    return count
end
function testCountPowerNumbers()
    assert(countPowerNumbers(99, 1) == 99)
    assert(countPowerNumbers(99, 3) == 7)
    assert(countPowerNumbers(99, 2) == 12)
    assert(countPowerNumbers(10, 1) == 10)
    assert(countPowerNumbers(10, 2) == 4)
    assert(countPowerNumbers(500, 1) == 500)
    assert(countPowerNumbers(500, 2) == 30)
    assert(countPowerNumbers(500, 3) == 13)
    assert(countPowerNumbers(1000, 1) == 1000)
    assert(countPowerNumbers(1000, 2) == 41)
    assert(countPowerNumbers(1000, 3) == 17)
    assert(countPowerNumbers(1000, 93) == 1)
    assert(countPowerNumbers(50, 2) == 10)
    assert(countPowerNumbers(50, 3) == 5)
    assert(countPowerNumbers(2, 3) == 1)
end

testCountPowerNumbers()