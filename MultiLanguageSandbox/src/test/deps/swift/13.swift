func kCloseNeighbors(_ nums: [Int], _ key: Int, _ k: Int) -> [Int] {
    var keyIndices = [Int]()
    // Step 1: Find all indices where nums[j] == key
    for (index, num) in nums.enumerated() {
        if num == key {
            keyIndices.append(index)
        }
    }
    
    var result = Set<Int>()
    // Step 2: For each key index, add all indices within k steps
    for j in keyIndices {
        let start = max(0, j - k)
        let end = min(nums.count - 1, j + k)
        for i in start...end {
            result.insert(i)
        }
    }
    
    // Convert the set to an array and sort it
    return Array(result).sorted()
}


func checkKCloseNeighbors(_ kCloseNeighbors: ([Int], Int, Int) -> [Int]) {
assert(kCloseNeighbors([1, 2, 3, 2, 4, 2, 5], 2, 2) == [0, 1, 2, 3, 4, 5, 6])
assert(kCloseNeighbors([1, 2, 3, 4, 5], 3, 1) == [1, 2, 3])
assert(kCloseNeighbors([1, 1, 1, 1, 1], 1, 0) == [0, 1, 2, 3, 4])
assert(kCloseNeighbors([5, 4, 3, 2, 1], 3, 2) == [0,1, 2, 3, 4])
assert(kCloseNeighbors([7, 7, 7, 7, 7], 7, 3) == [0, 1, 2, 3, 4])
assert(kCloseNeighbors([], 1, 1) == [])
assert(kCloseNeighbors([1, 2, 3, 4, 5, 6], 4, 1) == [2, 3, 4])
}

checkKCloseNeighbors(kCloseNeighbors)