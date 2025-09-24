func kthUniqueSmallestNumber(_ numbers: [Int], _ k: Int) -> String {
    // Get unique numbers by converting to a Set and back to an array
    let uniqueNumbers = Array(Set(numbers))
    
    // Sort the unique numbers in ascending order
    let sortedUniqueNumbers = uniqueNumbers.sorted()
    
    // Check if k is within the bounds of the sorted unique numbers
    if k > sortedUniqueNumbers.count || k <= 0 {
        return "NO RESULT"
    }
    
    // Return the kth smallest unique number (adjusting for 1-based index)
    return String(sortedUniqueNumbers[k - 1])
}


func testKthUniqueSmallestNumber() {
assert(kthUniqueSmallestNumber([5, 3, 9, 3, 5, 7, 2], 2) == "3")
assert(kthUniqueSmallestNumber([8, 1, 2, 2, 3, 4, 4, 6], 4) == "4")
assert(kthUniqueSmallestNumber([10, 20, 30], 4) == "NO RESULT")
assert(kthUniqueSmallestNumber([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 7) == "7")
assert(kthUniqueSmallestNumber([12, 15, 12, 18, 20, 22, 15, 18], 3) == "18")
assert(kthUniqueSmallestNumber([3, 3, 3, 3, 3], 1) == "3")
assert(kthUniqueSmallestNumber([40, 30, 20, 10], 2) == "20")
}

testKthUniqueSmallestNumber()