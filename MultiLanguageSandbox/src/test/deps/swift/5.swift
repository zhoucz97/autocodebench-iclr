func minCostToBuyCandies(_ costs: [Int]) -> Int {
    let sortedCosts = costs.sorted(by: >)
    var totalCost = 0
    for (index, cost) in sortedCosts.enumerated() {
        if index % 3 != 2 {
            totalCost += cost
        }
    }
    return totalCost
}


func checkMinCostToBuyCandies(_ minCostToBuyCandies: ([Int]) -> Int) {
assert(minCostToBuyCandies([1, 2, 3, 4]) == 8)
assert(minCostToBuyCandies([1, 2]) == 3)
assert(minCostToBuyCandies([1, 6, 3, 5, 2, 4]) == 16)
assert(minCostToBuyCandies([5, 5, 5, 5, 5, 5]) == 20)
assert(minCostToBuyCandies([7, 1, 2, 9, 6, 4]) == 22)
assert(minCostToBuyCandies([1]) == 1)
assert(minCostToBuyCandies([]) == 0)
}

checkMinCostToBuyCandies(minCostToBuyCandies)