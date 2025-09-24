
--[[
Count the number of valid coin toss sequences with no consecutive heads in n tosses.

Parameters:
- n (int): The number of coin toss sequences.

Returns:
unsigned long long: The count of valid sequences.

Examples:
extraNumber(1) -- returns 2
extraNumber(2) -- returns 3
]]

function count_valid_coin_toss_sequences(n)
    if n == 1 then
        return 2
    elseif n == 2 then
        return 3
    end
    
    local dp = {}
    dp[1] = 2
    dp[2] = 3
    
    for i = 3, n do
        dp[i] = dp[i-1] + dp[i-2]
    end
    
    return dp[n]
end
function testCountValidCoinTossSequences()
    assert(count_valid_coin_toss_sequences(1) == 2)
    assert(count_valid_coin_toss_sequences(2) == 3)
    assert(count_valid_coin_toss_sequences(3) == 5)
    assert(count_valid_coin_toss_sequences(4) == 8) -- Additional test
    assert(count_valid_coin_toss_sequences(5) == 13) -- Additional test
    -- Feel free to add more tests here
    assert(count_valid_coin_toss_sequences(40) == 267914296) -- Additional test
    assert(count_valid_coin_toss_sequences(39) == 165580141)
    assert(count_valid_coin_toss_sequences(38) == 102334155)
    -- print("All tests passed!")
end

testCountValidCoinTossSequences()