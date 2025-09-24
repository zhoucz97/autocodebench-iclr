func canFormPairs(_ nums: [Int]) -> Bool {
    var frequency = [Int: Int]()
    
    // Count the frequency of each number
    for num in nums {
        frequency[num] = (frequency[num] ?? 0) + 1
    }
    
    // Check if all frequencies are even
    for count in frequency.values {
        if count % 2 != 0 {
            return false
        }
    }
    
    return true
}


func checkCanFormPairs(_ canFormPairs: ([Int]) -> Bool) {
assert(canFormPairs([1, 2, 2, 1]) == true)
assert(canFormPairs([1, 2, 3, 4, 5, 6]) == false)
assert(canFormPairs([3, 3, 3, 3, 3, 3]) == true)
assert(canFormPairs([0, 0]) == true)
assert(canFormPairs([1, 1, 2, 2, 2, 2, 3, 3]) == true)
assert(canFormPairs([1, 2, 3, 4, 4, 3, 2, 1, 5]) == false)
assert(canFormPairs([]) == true)
}

checkCanFormPairs(canFormPairs)