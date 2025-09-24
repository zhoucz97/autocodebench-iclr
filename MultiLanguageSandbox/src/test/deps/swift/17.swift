func commonElementsInAllArrays(_ nums: [[Int]]) -> [Int] {
    guard !nums.isEmpty else { return [] }
    
    var commonElements = Set(nums[0])
    
    for array in nums.dropFirst() {
        let currentSet = Set(array)
        commonElements.formIntersection(currentSet)
        
        if commonElements.isEmpty {
            break
        }
    }
    
    return Array(commonElements).sorted()
}


func checkCommonElementsInAllArrays(_ commonElementsInAllArrays: ([[Int]]) -> [Int]) {
assert(commonElementsInAllArrays([[1, 2, 3], [2, 3, 4], [3, 4, 5]]) == [3])
assert(commonElementsInAllArrays([[1, 2], [2, 3, 4], [1, 2, 3]]) == [2])
assert(commonElementsInAllArrays([[1, 2, 3], [4, 5, 6], [7, 8, 9]]) == [])
assert(commonElementsInAllArrays([[10, 20], [20, 30], [20, 40]]) == [20])
assert(commonElementsInAllArrays([[1]]) == [1])
assert(commonElementsInAllArrays([]) == [])
assert(commonElementsInAllArrays([[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7]]) == [4])
}

checkCommonElementsInAllArrays(commonElementsInAllArrays)