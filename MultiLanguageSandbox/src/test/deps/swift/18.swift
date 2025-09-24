func performAlgorithmOnNums(_ nums: [Int]) -> Int {
    var currentNums = nums
    while currentNums.count > 1 {
        var nextNums = [Int]()
        for i in 0..<currentNums.count / 2 {
            let left = currentNums[2 * i]
            let right = currentNums[2 * i + 1]
            if i % 2 == 0 {
                nextNums.append(min(left, right))
            } else {
                nextNums.append(max(left, right))
            }
        }
        currentNums = nextNums
    }
    return currentNums.first!
}


func checkPerformAlgorithmOnNums(_ performAlgorithmOnNums: ([Int]) -> Int) {
assert(performAlgorithmOnNums([1, 3, 5, 2, 4, 6, 7, 8]) == 1)
assert(performAlgorithmOnNums([10, 100, 30, 20]) == 10)
assert(performAlgorithmOnNums([2, 4]) == 2)
assert(performAlgorithmOnNums([7]) == 7)
}

checkPerformAlgorithmOnNums(performAlgorithmOnNums)