func minimumTasteDifference(_ ingredients: [(sourness: Int, bitterness: Int)]) -> Int {
    var minDiff = Int.max
    
    // Iterate over all possible non-empty subsets using bitmasking
    let n = ingredients.count
    for mask in 1..<1 << n {
        var totalSourness = 1
        var totalBitterness = 0
        
        for i in 0..<n {
            if mask & (1 << i) != 0 {
                totalSourness *= ingredients[i].sourness
                totalBitterness += ingredients[i].bitterness
            }
        }
        
        let currentDiff = abs(totalSourness - totalBitterness)
        if currentDiff < minDiff {
            minDiff = currentDiff
        }
    }
    
    return minDiff
}


func testMinimumTasteDifference() {
assert(minimumTasteDifference([(3, 10)]) == 7)
assert(minimumTasteDifference([(3, 8), (5, 8)]) == 1)
assert(minimumTasteDifference([(1, 2), (2, 3), (3, 4)]) == 1)
assert(minimumTasteDifference([(2, 4), (3, 6)]) == 2)
assert(minimumTasteDifference([(1, 3), (2, 5), (4, 1)]) == 0)
}

testMinimumTasteDifference()