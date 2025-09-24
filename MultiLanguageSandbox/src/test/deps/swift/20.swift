func twoSum(_ nums: [Int], _ target: Int) -> [Int] {
    var numToIndex = [Int: Int]()
    
    for (index, num) in nums.enumerated() {
        let complement = target - num
        if let complementIndex = numToIndex[complement] {
            return [complementIndex, index]
        }
        numToIndex[num] = index
    }
    
    // The problem states there is exactly one solution, so this line is theoretically unreachable
    return []
}


func checkTwoSum(_ twoSum: ([Int], Int) -> [Int]) {
assert(twoSum([2, 7, 11, 15], 9) == [0, 1])
assert(twoSum([3, 2, 4], 6) == [1, 2])
assert(twoSum([3, 3], 6) == [0, 1])
assert(twoSum([1, 5, 4, 7], 8) == [0, 3])
assert(twoSum([1, 2, 3, 4], 7) == [2, 3])
assert(twoSum([4, 4], 8) == [0, 1])
assert(twoSum([-1, -2, -3, -4, -5], -8) == [2, 4])
}

checkTwoSum(twoSum)