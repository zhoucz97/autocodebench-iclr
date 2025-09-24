func countMealCombinations(_ dishes: [Int], _ budget: Int) -> Int {
    var dp = [Int](repeating: 0, count: budget + 1)
    dp[0] = 1 // Base case: one way to make sum 0 (select nothing)
    
    for dish in dishes {
        for i in stride(from: budget, through: dish, by: -1) {
            dp[i] += dp[i - dish]
        }
    }
    
    return dp[budget]
}


func check(_ countMealCombinations: ([Int], Int) -> Int) {
assert(countMealCombinations([1, 2, 3], 4) == 1)
assert(countMealCombinations([1, 1, 3, 4, 5], 5) == 4)
assert(countMealCombinations([2, 2, 2, 2], 8) == 1)
assert(countMealCombinations([1, 1, 1, 1, 1], 3) == 10)
assert(countMealCombinations([5, 5, 5, 5, 5], 10) == 10)
assert(countMealCombinations([10, 15, 20], 25) == 1)
assert(countMealCombinations([1, 3, 4, 6], 9) == 1)
}

check(countMealCombinations)