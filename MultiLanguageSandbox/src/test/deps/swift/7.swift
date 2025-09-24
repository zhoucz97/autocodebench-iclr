func findFinalValue(_ nums: [Int], _ original: Int) -> Int {
    var current = original
    while nums.contains(current) {
        current *= 2
    }
    return current
}


func checkFindFinalValue(_ findFinalValue: ([Int], Int) -> Int) {
assert(findFinalValue([5, 3, 6, 1, 12], 3) == 24)
assert(findFinalValue([2, 7, 9], 4) == 4)
assert(findFinalValue([1, 2, 4, 8, 16], 1) == 32)
assert(findFinalValue([10, 20, 30, 40], 10) == 80)
assert(findFinalValue([5, 10, 20, 40], 2) == 2)
assert(findFinalValue([], 1) == 1)
assert(findFinalValue([100, 200, 300], 100) == 400)
}

checkFindFinalValue(findFinalValue)