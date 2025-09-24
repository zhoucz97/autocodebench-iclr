
--[[
Given a permutation q of n elements and an integer k, find the number of permutations p of n elements
such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by
dividing p into exactly k non-empty contiguous segments and sorting each segment (that is, choose k-1 
breakpoints 1 <= x1 < x2 < ... < x(k-1) < n, and divide p into [1, x1], (x1, x2], ..., (x(k-1), n]).
The result should be modulo 998244353. 

    >>> countPermutations(2, 1, {1, 2})
    2
--]]

local MOD = 998244353

-- Precompute factorials up to n (assuming n is up to a reasonable limit, say 2000)
local max_n = 2000
local fact = {}
fact[0] = 1
for i = 1, max_n do
    fact[i] = (fact[i-1] * i) % MOD
end

function countPermutations(n, k, q)
    -- Check if q can be split into k increasing runs
    local runs = 1
    for i = 2, n do
        if q[i] < q[i-1] then
            runs = runs + 1
        end
    end
    if runs ~= k then
        return 0
    end
    
    -- Now, find the positions where the runs change (i.e., q[i] < q[i-1])
    local split_positions = {}
    for i = 2, n do
        if q[i] < q[i-1] then
            table.insert(split_positions, i-1)
        end
    end
    
    -- The number of ways to choose k-1 splits from the possible split positions
    -- But since the runs are fixed by q, the only way is to use exactly those split positions
    -- So the answer is the product of the factorials of the lengths of each run
    
    local total = 1
    local prev = 1
    for _, pos in ipairs(split_positions) do
        local length = pos - prev + 1
        total = (total * fact[length]) % MOD
        prev = pos + 1
    end
    -- Add the last run
    local last_length = n - prev + 1
    total = (total * fact[last_length]) % MOD
    
    return total
end

-- Example usage:
print(countPermutations(2, 1, {1, 2}))  -- Output: 2
local function testCountPermutations()
    local q1 = {1, 2}
    assert(countPermutations(2, 1, q1) == 2)

    local q2 = {3, 1, 2}
    assert(countPermutations(3, 3, q2) == 1)

    local q3 = {1, 2, 3, 6, 5, 4}
    assert(countPermutations(6, 3, q3) == 13)

    local q4 = {1, 2, 3, 4, 5, 6}
    assert(countPermutations(6, 1, q4) == 720)

    local q5 = {1, 2, 5, 3, 4, 5}
    assert(countPermutations(6, 3, q5) == 0)

    local q6 = {1, 2, 3, 4, 5, 6, 7, 8, 9}
    assert(countPermutations(9, 9, q6) == 1)

    local q7 = {1, 2, 3, 4, 5, 6, 7, 9, 8}
    assert(countPermutations(9, 2, q7) == 29093)
end

testCountPermutations()