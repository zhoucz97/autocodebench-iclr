
--[[
Count the number of different permutation schemes for a binary string of length n,
where the number of '1's is m and the number of '0's is n - m.

Parameters:
- n (int): Length of the binary string.
- m (int): Number of '1's in the binary string.

Returns:
int: The total number of different permutation schemes.
--]]

function count_permutations_of_binary_string(n, m)
    -- Handle edge cases
    if m < 0 or m > n then
        return 0
    end
    
    -- Optimization: C(n, m) == C(n, n-m), so choose the smaller m to minimize computations
    m = math.min(m, n - m)
    
    local result = 1
    for i = 1, m do
        result = result * (n - m + i) / i
    end
    
    return result
end
do
    assert(count_permutations_of_binary_string(2, 0) == 2)
    assert(count_permutations_of_binary_string(2, 1) == 0)
    assert(count_permutations_of_binary_string(3, 0) == 0)
    assert(count_permutations_of_binary_string(3, 1) == 3)
    assert(count_permutations_of_binary_string(3, 2) == 0)
    assert(count_permutations_of_binary_string(30, 2) == 145422675)
    assert(count_permutations_of_binary_string(4, 2) == 4)
    assert(count_permutations_of_binary_string(5, 5) == 1)
    assert(count_permutations_of_binary_string(33, 17) == 13884156)
    assert(count_permutations_of_binary_string(1000, 1000) == 1)
    -- Add more test cases if necessary
end