func countDivisiblePairs(_ nums: [Int], _ k: Int) -> Int {
    var count = 0
    let n = nums.count
    
    for i in 0..<n {
        for j in i+1..<n {
            if nums[i] == nums[j] && (i * j) % k == 0 {
                count += 1
            }
        }
    }
    
    return count
}


func checkCountDivisiblePairs(_ countDivisiblePairs: ([Int], Int) -> Int) {
assert(countDivisiblePairs([1, 2, 3, 1, 2, 3], 3) == 1)
assert(countDivisiblePairs([1, 1, 1, 1], 1) == 6)
assert(countDivisiblePairs([1, 2, 3, 4, 5, 6], 7) == 0)
assert(countDivisiblePairs([1, 2, 2, 1, 2], 2) == 4)
assert(countDivisiblePairs([4, 4, 4, 4], 4) == 3)
assert(countDivisiblePairs([], 5) == 0)
assert(countDivisiblePairs([2, 2, 2, 2, 2], 3) == 7 )
}

checkCountDivisiblePairs(countDivisiblePairs)