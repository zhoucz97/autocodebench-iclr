func isValidMatrix(_ matrix: [[Int]]) -> Bool {
    let n = matrix.count
    guard n > 0 else { return false } // handle empty matrix case, though problem says n x n
    
    // Check each row
    for row in matrix {
        var seen = Set<Int>()
        for num in row {
            if num < 1 || num > n || seen.contains(num) {
                return false
            }
            seen.insert(num)
        }
        if seen.count != n {
            return false
        }
    }
    
    // Check each column
    for col in 0..<n {
        var seen = Set<Int>()
        for row in 0..<n {
            let num = matrix[row][col]
            if num < 1 || num > n || seen.contains(num) {
                return false
            }
            seen.insert(num)
        }
        if seen.count != n {
            return false
        }
    }
    
    return true
}


func checkIsValidMatrix(_ isValidMatrix: ([[Int]]) -> Bool) {
assert(isValidMatrix([[1,2,3],[3,1,2],[2,3,1]]) == true)
assert(isValidMatrix([[1,2,3],[3,2,1],[1,2,3]]) == false)
assert(isValidMatrix([[1]]) == true)
assert(isValidMatrix([[1,2],[2,1]]) == true)
assert(isValidMatrix([[1,2],[1,2]]) == false)
assert(isValidMatrix([[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]) == true)
assert(isValidMatrix([[4,3,2,1],[3,2,1,4],[2,1,4,3],[1,4,3,2]]) == true)
assert(isValidMatrix([[1,2,3],[3,1,2],[3,2,1]]) == false)
}

checkIsValidMatrix(isValidMatrix)