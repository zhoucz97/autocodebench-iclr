
" Given n positive integers representing the count of each number from 1 to n, 
" find the maximum sum of the mode (most frequent element) for all prefixes of 
" a sequence constructed from these numbers. The mode is the largest number among 
" the most frequent elements in a sequence. A sequence that reaches its maximum value 
" is (3,2,3,1,2,2) when called with maxModeSum(3, [1, 3, 2]) which returns 17.

function MaxModeSum(n, counts)
    " Create a list of tuples (number, count) sorted in descending order of number
    let numbers = []
    for i in range(1, a:n)
        call add(numbers, [i, a:counts[i - 1]])
    endfor
    " Sort the numbers in descending order to prioritize larger numbers
    call sort(numbers, {a, b -> b[0] - a[0]})
    
    let sequence = []
    for num in numbers
        let sequence += repeat([num[0]], num[1])
    endfor
    
    let sum = 0
    let freq = {}
    let max_freq = 0
    let current_mode = 0
    
    for num in sequence
        " Update frequency of the current number
        if has_key(freq, num)
            let freq[num] += 1
        else
            let freq[num] = 1
        endif
        
        " Check if this number's frequency exceeds the current max frequency
        if freq[num] > max_freq
            let max_freq = freq[num]
            let current_mode = num
        elseif freq[num] == max_freq && num > current_mode
            let current_mode = num
        endif
        
        let sum += current_mode
    endfor
    
    return sum
endfunction

if assert_equal(MaxModeSum(3, [1, 3, 2]), 17) | cq | endif
if assert_equal(MaxModeSum(4, [4, 1, 2, 3]), 37) | cq | endif
if assert_equal(MaxModeSum(2, [1, 1]), 4) | cq | endif
if assert_equal(MaxModeSum(5, [1, 2, 3, 4, 5]), 75) | cq | endif
if assert_equal(MaxModeSum(1, [100000]), 100000) | cq | endif
if assert_equal(MaxModeSum(5, [5, 3, 2, 4, 1]), 62) | cq | endif
if assert_equal(MaxModeSum(3, [100000, 100000, 100000]), 900000) | cq | endif
if assert_equal(MaxModeSum(3, [2, 2, 5]), 27) | cq | endif
if assert_equal(MaxModeSum(4, [4, 4, 4, 4]), 64) | cq | endif
if assert_equal(MaxModeSum(6, [1, 2, 3, 4, 5, 6]), 126) | cq | endif
if assert_equal(MaxModeSum(3, [3, 1, 2]), 16) | cq | endif

exit(0)