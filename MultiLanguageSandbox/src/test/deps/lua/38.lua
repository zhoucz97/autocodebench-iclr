
--[[
Given n positive integers representing the count of each number from 1 to n,
find the maximum sum of the mode (most frequent element) for all prefixes of
a sequence constructed from these numbers. The mode is the largest number among
the most frequent elements in a sequence.
    >>> maxModeSum(3, {1, 2, 3})
    17
    A sequence that reaches its maximum value is (3,2,3,1,2,2).
]]--

function maxModeSum(n, counts)
    local total = 0
    for i = 1, n do
        total = total + counts[i]
    end
    
    -- We need to construct a sequence that maximizes the sum of modes of all prefixes.
    -- The approach is to arrange numbers in a way that higher numbers are placed as much as possible early or in positions where they can be the mode.
    
    -- The optimal sequence seems to be constructed by placing the highest numbers first, but interleaving them to maximize the mode sum.
    -- For example, for counts {1, 2, 3}, the sequence (3,2,3,1,2,2) gives modes:
    -- 1: 3
    -- 2: 3 (3 appears once, 2 once)
    -- 3: 3 (3 appears twice, 2 once)
    -- 4: 3 (3 appears twice, 2 once, 1 once)
    -- 5: 3 (3 appears twice, 2 twice, 1 once)
    -- 6: 2 (3 appears twice, 2 twice, 1 once)
    -- Sum: 3 + 3 + 3 + 3 + 3 + 2 = 17
    
    -- The general approach is to arrange numbers in descending order, but with some interleaving.
    -- For each step, we pick the highest available number that can contribute to the mode.
    
    -- Create a list of numbers with their counts, sorted in descending order.
    local nums = {}
    for i = 1, n do
        table.insert(nums, {num = i, count = counts[i]})
    end
    table.sort(nums, function(a, b) return a.num > b.num end)
    
    local sequence = {}
    local remaining = {}
    for _, v in ipairs(nums) do
        table.insert(remaining, {num = v.num, count = v.count})
    end
    
    -- The strategy is to always pick the highest available number that hasn't been fully used yet.
    -- But we need to ensure that when we add a number, it becomes the mode for as many prefixes as possible.
    -- This is similar to a greedy approach where we try to maximize the mode at each step.
    
    -- Initialize a frequency table and the current mode.
    local freq = {}
    local current_mode = 0
    local mode_sum = 0
    
    -- We'll process the numbers in a way that for each step, we pick the highest possible number that can be the mode.
    -- This requires a more sophisticated approach than simple sorting.
    
    -- Alternative approach: the optimal sequence is constructed by placing the highest numbers first, but in a specific pattern.
    -- For each number from highest to lowest, we place it as much as possible, but interleaved with others to maximize the mode sum.
    
    -- Let's try to build the sequence step by step, always choosing the highest possible number that can be the mode for the current prefix.
    
    local seq = {}
    local freq_table = {}
    local max_freq = 0
    local current_max_num = 0
    
    -- We'll use a priority queue (or simulate it) to always pick the highest available number with the highest remaining count.
    -- But since Lua doesn't have a built-in priority queue, we'll manage it with a table and sorting.
    
    -- Create a list of available numbers, sorted in descending order, and their remaining counts.
    local available = {}
    for i = 1, n do
        if counts[i] > 0 then
            table.insert(available, {num = i, count = counts[i]})
        end
    end
    table.sort(available, function(a, b) return a.num > b.num end)
    
    -- The algorithm is to, for each step, pick the highest available number that can be the mode for the new prefix.
    -- This means that after adding the number, it should have the highest frequency (or tie for highest, choosing the larger number).
    
    -- Initialize frequency table.
    local freq = {}
    for i = 1, n do
        freq[i] = 0
    end
    
    local mode_sum = 0
    
    -- The total length of the sequence is the sum of all counts.
    local total_length = 0
    for i = 1, n do
        total_length = total_length + counts[i]
    end
    
    for step = 1, total_length do
        -- Find the best number to add next.
        -- The best number is the highest available number that, when added, makes it the mode (or ties for mode with a higher number).
        local best_num = nil
        local best_count = -1
        
        -- Iterate through available numbers in descending order.
        for _, item in ipairs(available) do
            if item.count > 0 then
                -- Temporarily assume we add this number.
                local temp_freq = freq[item.num] + 1
                -- Check if this number would be the mode after addition.
                -- The mode is the number with the highest frequency, and in case of tie, the largest number.
                local is_mode = true
                for num = 1, n do
                    if num ~= item.num and freq[num] + (num == item.num and 1 or 0) >= temp_freq then
                        -- There's another number with frequency >= temp_freq, and it's larger than item.num?
                        -- No, because we're checking in descending order, so item.num is the highest possible.
                        -- So if any other number has frequency >= temp_freq, then item.num cannot be the mode unless it's larger.
                        -- But since we're processing in descending order, item.num is the largest possible, so if frequencies are equal, it's the mode.
                        -- So the condition is that no other number has a higher frequency than temp_freq, or if equal, the number is larger.
                        -- So item.num is the mode if:
                        -- for all other numbers, their frequency (after adding item.num) is < temp_freq, or equal but item.num is larger.
                        -- Since we're processing in descending order, the first such item.num encountered will satisfy this.
                        is_mode = false
                        break
                    end
                end
                if is_mode then
                    best_num = item.num
                    best_count = item.count
                    break
                end
            end
        end
        
        if not best_num then
            -- If no best_num found (shouldn't happen), pick the highest available.
            for _, item in ipairs(available) do
                if item.count > 0 then
                    best_num = item.num
                    best_count = item.count
                    break
                end
            end
        end
        
        -- Add best_num to the sequence.
        table.insert(seq, best_num)
        freq[best_num] = freq[best_num] + 1
        
        -- Update available counts.
        for i, item in ipairs(available) do
            if item.num == best_num then
                item.count = item.count - 1
                if item.count == 0 then
                    table.remove(available, i)
                end
                break
            end
        end
        
        -- Compute the current mode.
        local current_max_freq = 0
        local current_mode_num = 0
        for num = 1, n do
            if freq[num] > current_max_freq then
                current_max_freq = freq[num]
                current_mode_num = num
            elseif freq[num] == current_max_freq and num > current_mode_num then
                current_mode_num = num
            end
        end
        mode_sum = mode_sum + current_mode_num
    end
    
    return mode_sum
end
function testMaxModeSum()
    assert(maxModeSum(3, {1, 3, 2}) == 17)
    assert(maxModeSum(4, {4, 1, 2, 3}) == 37)
    assert(maxModeSum(2, {1, 1}) == 4)
    assert(maxModeSum(5, {1, 2, 3, 4, 5}) == 75)
    assert(maxModeSum(1, {100000}) == 100000)
    assert(maxModeSum(5, {5, 3, 2, 4, 1}) == 62)
    assert(maxModeSum(3, {100000, 100000, 100000}) == 900000)
    assert(maxModeSum(3, {2, 2, 5}) == 27)
    assert(maxModeSum(4, {4, 4, 4, 4}) == 64)
    assert(maxModeSum(6, {1, 2, 3, 4, 5, 6}) == 126)
    assert(maxModeSum(3, {3, 1, 2}) == 16)
end

testMaxModeSum()