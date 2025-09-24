func countElementsWithStrictBounds(_ nums: [Int]) -> Int {
    guard !nums.isEmpty else { return 0 }
    let minVal = nums.min()!
    let maxVal = nums.max()!
    
    if minVal == maxVal {
        return 0
    }
    
    var count = 0
    for num in nums {
        if num > minVal && num < maxVal {
            count += 1
        }
    }
    return count
}


func checkCountElementsWithStrictBounds(_ countElementsWithStrictBounds: ([Int]) -> Int) {
assert(countElementsWithStrictBounds([2, 1, 3]) == 1)
assert(countElementsWithStrictBounds([1, 2, 3]) == 1)
assert(countElementsWithStrictBounds([1, 1, 1]) == 0)
assert(countElementsWithStrictBounds([3, 3, 5, 2, 2, 7, 4]) == 4)
assert(countElementsWithStrictBounds([10, 5, 9, 1, 7, 6, 8]) == 5)
assert(countElementsWithStrictBounds([-2, -1, 0, 1, 2]) == 3)
assert(countElementsWithStrictBounds([]) == 0)
}

checkCountElementsWithStrictBounds(countElementsWithStrictBounds)