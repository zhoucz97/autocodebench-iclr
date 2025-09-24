func findUniqueElements(nums1: [Int], nums2: [Int]) -> [[Int]] {
    // Convert arrays to sets to remove duplicates and find unique elements
    let set1 = Set(nums1)
    let set2 = Set(nums2)
    
    // Find elements unique to nums1 (in set1 but not in set2)
    let uniqueToNums1 = set1.subtracting(set2).sorted()
    
    // Find elements unique to nums2 (in set2 but not in set1)
    let uniqueToNums2 = set2.subtracting(set1).sorted()
    
    return [uniqueToNums1, uniqueToNums2]
}


func checkFindUniqueElements(_ findUniqueElements: ([Int], [Int]) -> [[Int]]) {
assert(findUniqueElements([1, 2, 3], [2, 3, 4]) == [[1], [4]])
assert(findUniqueElements( [1, 1, 2, 3], [3, 4, 4, 5]) == [[1, 2], [4, 5]])
assert(findUniqueElements( [1, 2, 2], [1, 1, 1]) == [[2], []])
assert(findUniqueElements( [], [1, 2, 3]) == [[], [1, 2, 3]])
assert(findUniqueElements( [4, 5, 6], [4, 5, 6]) == [[], []])
assert(findUniqueElements( [10], [10, 20]) == [[], [20]])
}

checkFindUniqueElements(findUniqueElements)