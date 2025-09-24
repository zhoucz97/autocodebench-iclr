
--[[ 
Given n doors arranged in a circle, the player starts in front of door 1. 
Each turn, the player can choose a number i and pay a cost C_i to move i steps to the right 
and then open the door at that position. It is guaranteed that C_i >= C_{i+1} for 1 <= i < n.
The task is to determine the minimum total cost required to open all doors.

Example:
    >>> minTotalCost(3, {1, 1, 1})
    3
]]

function minTotalCost(n, C)
    local totalCost = 0
    local currentPos = 1  -- starts at door 1 (1-based index)
    local opened = {}
    for i = 1, n do
        opened[i] = false
    end
    opened[1] = true  -- door 1 is already "opened" by starting there
    
    while true do
        local allOpened = true
        for i = 1, n do
            if not opened[i] then
                allOpened = false
                break
            end
        end
        if allOpened then
            break
        end
        
        -- Find the largest i such that moving i steps from currentPos reaches an unopened door
        local maxI = 0
        for i = n, 1, -1 do
            local nextPos = (currentPos + i - 1) % n + 1  -- convert to 1-based modulo
            if not opened[nextPos] then
                maxI = i
                break
            end
        end
        
        -- Move maxI steps
        totalCost = totalCost + C[maxI]
        currentPos = (currentPos + maxI - 1) % n + 1
        opened[currentPos] = true
    end
    
    return totalCost
end
function testMinTotalCost()
	assert(minTotalCost(5, {4, 3, 3, 3, 3}) == 15)
	assert(minTotalCost(3, {1, 1, 1}) == 3)
	assert(minTotalCost(4, {5, 4, 3, 2}) == 11)
	assert(minTotalCost(4, {100, 99, 98, 97}) == 391)
	assert(minTotalCost(6, {10, 9, 8, 7, 6, 5}) == 35)
	assert(minTotalCost(7, {2, 2, 2, 2, 2, 2, 2}) == 14)
	assert(minTotalCost(8, {9, 7, 7, 7, 7, 7, 7, 7}) == 56)
	assert(minTotalCost(9, {3, 2, 2, 2, 2, 2, 2, 2, 2}) == 18)
	assert(minTotalCost(10, {6, 5, 5, 5, 5, 5, 5, 5, 5, 5}) == 50)
	assert(minTotalCost(11, {8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}) == 11)
end

testMinTotalCost()