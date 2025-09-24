func adjustChessPieces(_ counts: [Int]) -> [Int] {
    // Define the required counts for a standard chess set
    let requiredCounts = [1, 1, 2, 2, 2, 8]
    
    // Calculate the difference between the input counts and the required counts
    var adjustments = [Int]()
    for (inputCount, requiredCount) in zip(counts, requiredCounts) {
        adjustments.append(requiredCount - inputCount)
    }
    
    return adjustments
}


func testAdjustChessPieces() {
assert(adjustChessPieces([1, 1, 2, 2, 2, 8]) == [0, 0, 0, 0, 0, 0], "Test Case 1 Failed")
assert(adjustChessPieces([0, 2, 1, 2, 2, 5]) == [1, -1, 1, 0, 0, 3], "Test Case 2 Failed")
assert(adjustChessPieces([1, 0, 2, 1, 3, 8]) == [0, 1, 0, 1, -1, 0], "Test Case 3 Failed")
assert(adjustChessPieces([0, 1, 0, 0, 0, 0]) == [1, 0, 2, 2, 2, 8], "Test Case 4 Failed")
assert(adjustChessPieces([2, 2, 3, 3, 3, 10]) == [-1, -1, -1, -1, -1, -2], "Test Case 5 Failed")
}

testAdjustChessPieces()