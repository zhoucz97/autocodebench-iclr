
--[[
Given a rectangle paper of size n x m placed on a plane Cartesian coordinate system with its bottom-left corner at (0,0) and top-right corner at (n,m),
you need to calculate the expected number of operations to cut the paper such that the remaining area is less than k. 
In each operation, a line is randomly chosen that is parallel to the axes, passes through points with integer coordinates, 
and cuts through (not just touching the edge) the paper. The bottom or right part of the paper along this line is then discarded.
The answer should be modulo 10^9+7.

For example:
    >>> expectedCuts(3, 3, 4)
    3
    >>> expectedCuts(5, 5, 12)
    2
--]]

local MOD = 10^9 + 7

function expectedCuts(n, m, k)
    local memo = {}
    
    local function dp(i, j)
        if i * j < k then
            return 0
        end
        local key = tostring(i) .. "," .. tostring(j)
        if memo[key] ~= nil then
            return memo[key]
        end
        
        local total = 0
        local count = 0
        
        -- Horizontal cuts: i can be split into 1..i-1 (since the cut must pass through integer coordinates and cut the paper)
        for x = 1, i - 1 do
            total = (total + dp(x, j)) % MOD
            count = (count + 1) % MOD
        end
        
        -- Vertical cuts: j can be split into 1..j-1
        for y = 1, j - 1 do
            total = (total + dp(i, y)) % MOD
            count = (count + 1) % MOD
        end
        
        -- The expected value is (total + count) / (count), but since we add 1 for each cut scenario (the current cut plus the expected后续 cuts)
        -- Wait, no. The total is sum of (1 + dp(x,j) for horizontal cuts) and (1 + dp(i,y) for vertical cuts). So total is sum of (1 + dp_val) for each possible cut.
        -- So the previous approach was incorrect. Let me re-express:
        
        total = 0
        count = 0
        
        -- Horizontal cuts: for each possible x in 1..i-1, the cut splits into x*j and (i-x)*j. We keep x*j (assuming we discard the right part)
        -- But the problem says: "the bottom or right part of the paper along this line is then discarded." So for a horizontal cut at x, the remaining is x*j (if we discard the bottom) or (i-x)*j (if we discard the top). But the problem statement is a bit ambiguous. However, the sample inputs suggest that for a 3x3 paper and k=4, the answer is 3. Let's see:
        -- For 3x3, area 9 >=4. Possible cuts:
        -- Horizontal at 1: remaining 1x3 (area 3 <4) or 2x3 (area 6 >=4). If we choose to discard the bottom (1x3), then next step is 2x3. Expected cuts is 1 + dp(2,3).
        -- Horizontal at 2: remaining 2x3 or 1x3. If discard bottom, 2x3. Expected 1 + dp(2,3).
        -- Vertical at 1: remaining 3x1 or 3x2. If discard left, 3x2. Expected 1 + dp(3,2).
        -- Vertical at 2: remaining 3x2 or 3x1. If discard left, 3x2. Expected 1 + dp(3,2).
        -- So total expected is average of (1 + dp(2,3), 1 + dp(2,3), 1 + dp(3,2), 1 + dp(3,2)) = 1 + average(dp(2,3), dp(2,3), dp(3,2), dp(3,2)) = 1 + (dp(2,3) + dp(2,3) + dp(3,2) + dp(3,2))/4.
        -- So the correct approach is to consider all possible cuts, each contributing (1 + dp(new_i, new_j)), and average them.
        
        -- Re-initialize total and count
        total = 0
        count = 0
        
        -- Horizontal cuts: for each x in 1..i-1, the cut can leave x*j or (i-x)*j. Assuming we always leave the larger piece (but the problem says "bottom or right part is discarded", so for horizontal cut at x, if we discard bottom, leave x*j; if discard top, leave (i-x)*j. But the problem doesn't specify which part is discarded. The samples suggest that the answer is consistent if we assume that after each cut, the remaining part is the one not discarded. So for a horizontal cut at x, the remaining is either x*j or (i-x)*j, depending on which part is discarded. But the problem statement says: "the bottom or right part of the paper along this line is then discarded." So for a horizontal cut, the line is y = x (assuming x is the y-coordinate). The bottom part is below the line (y < x), so discarding bottom leaves the top part (y >=x), which is a rectangle of size (i -x) * j. Similarly, discarding the top would leave the bottom part, size x*j. But the problem says "the bottom or right part is discarded", implying that for horizontal cuts, we can choose to discard either bottom or top. But the samples suggest that the answer is the same regardless of the choice. So perhaps the problem implies that for each cut, we randomly choose to discard either the bottom or the top (for horizontal cuts) or the left or the right (for vertical cuts). But the samples don't provide enough information. Given the ambiguity, I'll proceed under the assumption that for each cut, we randomly choose to discard one of the two possible parts (bottom or top for horizontal, left or right for vertical), and the expected value is the average over all possible choices.
        
        -- So for each possible cut, there are two choices (which part to discard), leading to two possible next states. The total expected is the average over all possible cuts and choices.
        
        -- Horizontal cuts: for each x in 1..i-1, two choices: discard bottom (leave (i-x)*j) or discard top (leave x*j). Each choice has probability 1/2.
        for x = 1, i - 1 do
            local option1 = dp(i - x, j)  -- discard bottom, leave (i-x)*j
            local option2 = dp(x, j)      -- discard top, leave x*j
            local avg = (option1 + option2) * 2  -- because each option is multiplied by 1/2, but we'll divide later
            total = (total + avg) % MOD
            count = (count + 2) % MOD  -- two choices per x
        end
        
        -- Vertical cuts: for each y in 1..j-1, two choices: discard left (leave i*(j-y)) or discard right (leave i*y)
        for y = 1, j - 1 do
            local option1 = dp(i, j - y)  -- discard left, leave i*(j-y)
            local option2 = dp(i, y)      -- discard right, leave i*y
            local avg = (option1 + option2) * 2
            total = (total + avg) % MOD
            count = (count + 2) % MOD
        end
        
        if count == 0 then
            memo[key] = 0
            return 0
        end
        
        -- The total is sum over all cuts and choices of (1 + dp_next). So total is sum (1 + dp_next) for each scenario.
        -- But in the code above, for each cut scenario (horizontal or vertical), we have two choices, each contributing (1 + dp_next). So total is sum over all scenarios of (1 + dp_next), and count is the total number of scenarios (2 per horizontal cut, 2 per vertical cut).
        -- So total is sum (1 + dp_next) for each scenario. But in the code, for each x in horizontal cuts, we add (option1 + option2)*2, which is (dp(i-x,j) + dp(x,j))*2. But each of these corresponds to a scenario where we make a cut and then have dp_next. The 1 (for the current cut) is missing. So the correct approach is to for each scenario (each cut and each choice), add (1 + dp_next). So for each x in horizontal cuts, there are two scenarios: cut at x and discard bottom (then 1 + dp(i-x,j)), or cut at x and discard top (1 + dp(x,j)). So total increases by (1 + dp(i-x,j) + 1 + dp(x,j)) = 2 + dp(i-x,j) + dp(x,j). Similarly for vertical cuts.
        -- So the previous code missed adding the 1 for each scenario. Let me correct that.
        
        -- Re-initialize total and count again
        total = 0
        count = 0
        
        -- Horizontal cuts: for each x in 1..i-1, two scenarios: cut at x and discard bottom (1 + dp(i-x,j)), or cut at x and discard top (1 + dp(x,j))
        for x = 1, i - 1 do
            local val1 = (1 + dp(i - x, j)) % MOD
            local val2 = (1 + dp(x, j)) % MOD
            total = (total + val1 + val2) % MOD
            count = (count + 2) % MOD
        end
        
        -- Vertical cuts: for each y in 1..j-1, two scenarios: cut at y and discard left (1 + dp(i, j-y)), or cut at y and discard right (1 + dp(i, y))
        for y = 1, j - 1 do
            local val1 = (1 + dp(i, j - y)) % MOD
            local val2 = (1 + dp(i, y)) % MOD
            total = (total + val1 + val2) % MOD
            count = (count + 2) % MOD
        end
        
        if count == 0 then
            memo[key] = 0
            return 0
        end
        
        -- The expected value is total / count. Since we're working modulo MOD, we need to multiply by the modular inverse of count.
        local inv_count = modinv(count, MOD)
        local res = (total * inv_count) % MOD
        memo[key] = res
        return res
    end
    
    -- Function to compute modular inverse using Fermat's little theorem, since MOD is prime.
    local function modinv(a, mod)
        return pow(a, mod - 2, mod)
    end
    
    -- Function to compute a^b mod m
    local function pow(a, b, m)
        if b == 0 then return 1 end
        local half = pow(a, b // 2, m)
        local res = (half * half) % m
        if b % 2 == 1 then
            res = (res * a) % m
        end
        return res
    end
    
    return dp(n, m)
end

-- Test cases
print(expectedCuts(3, 3, 4))  -- Expected output: 3
print(expectedCuts(5, 5, 12)) -- Expected output: 2
function testExpectedCuts()
    assert(expectedCuts(2, 4, 10) == 0)
    assert(expectedCuts(2, 4, 8) == 1)
    assert(expectedCuts(2, 4, 2) == 833333342)
    assert(expectedCuts(2, 4, 6) == 250000003)
    assert(expectedCuts(3, 3, 4) == 666666673)
    assert(expectedCuts(5, 5, 12) == 666666673)
    assert(expectedCuts(6, 7, 20) == 722222229)
    assert(expectedCuts(8, 8, 30) == 72727275)
    assert(expectedCuts(10, 10, 50) == 714285721)
    assert(expectedCuts(1, 10, 5) == 945634929)
    assert(expectedCuts(10, 1, 5) == 945634929)
end

testExpectedCuts()