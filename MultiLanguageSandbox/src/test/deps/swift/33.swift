func maxSubArraySum(_ nums: [Int]) -> Int {
    guard !nums.isEmpty else { return 0 }
    
    var maxCurrent = nums[0]
    var maxGlobal = nums[0]
    
    for i in 1..<nums.count {
        maxCurrent = max(nums[i], maxCurrent + nums[i])
        if maxCurrent > maxGlobal {
            maxGlobal = maxCurrent
        }
    }
    
    return maxGlobal
}


func check(_ maxSubArraySum: ([Int]) -> Int) {
assert(maxSubArraySum([1, -2, 3, 4, -1, 2, 1, -5, 4]) == 9)
assert(maxSubArraySum([-2, -3, -1]) == -1)
assert(maxSubArraySum([5, -3, 5]) == 7)
assert(maxSubArraySum([-1, -2, -3, -4]) == -1)
assert(maxSubArraySum([2, 3, -2, 5, -3]) == 8)
assert(maxSubArraySum([10, -11, 12]) == 12)
assert(maxSubArraySum([-2, 1, -3, 4, -1, 2, 1, -5, 4]) == 6)
}

check(maxSubArraySum)