func reorderArray(_ nums: [Int]) -> [Int] {
    guard !nums.isEmpty else { return [] }
    
    // Separate elements at even and odd indices
    var evenIndices = [Int]()
    var oddIndices = [Int]()
    
    for (index, num) in nums.enumerated() {
        if index % 2 == 0 {
            evenIndices.append(num)
        } else {
            oddIndices.append(num)
        }
    }
    
    // Sort even indices in non-decreasing order
    evenIndices.sort()
    
    // Sort odd indices in non-increasing order
    oddIndices.sort(by: >)
    
    // Reconstruct the array
    var result = [Int](repeating: 0, count: nums.count)
    var evenIndex = 0
    var oddIndex = 0
    
    for i in 0..<nums.count {
        if i % 2 == 0 {
            result[i] = evenIndices[evenIndex]
            evenIndex += 1
        } else {
            result[i] = oddIndices[oddIndex]
            oddIndex += 1
        }
    }
    
    return result
}


func checkReorderArray(_ reorderArray: ([Int]) -> [Int]) {
assert(reorderArray([4, 1, 2, 3]) == [2, 3, 4, 1])
assert(reorderArray([6, 5, 4, 3, 2, 1]) == [2, 5, 4, 3, 6, 1])
assert(reorderArray([1, 2, 3, 4, 5, 6]) == [1, 6, 3, 4, 5, 2])
assert(reorderArray([10, 9, 8, 7, 6, 5, 4, 3, 2, 1]) == [2, 9, 4, 7, 6, 5, 8, 3, 10, 1])
assert(reorderArray([1]) == [1])
assert(reorderArray([1, 2]) == [1, 2])
assert(reorderArray([]) == [])
}

checkReorderArray(reorderArray)