func findSmallestWordInMatrix(_ matrix: [[Character]]) -> String {
    var smallestWord = ""
    
    // Check horizontal words
    for row in matrix {
        var currentWord = ""
        for char in row {
            if char == "#" {
                if currentWord.count >= 2 {
                    if smallestWord.isEmpty || currentWord < smallestWord {
                        smallestWord = currentWord
                    }
                }
                currentWord = ""
            } else {
                currentWord.append(char)
            }
        }
        // Check the last segment in the row
        if currentWord.count >= 2 {
            if smallestWord.isEmpty || currentWord < smallestWord {
                smallestWord = currentWord
            }
        }
    }
    
    // Check vertical words
    let columns = matrix[0].count
    for col in 0..<columns {
        var currentWord = ""
        for row in 0..<matrix.count {
            let char = matrix[row][col]
            if char == "#" {
                if currentWord.count >= 2 {
                    if smallestWord.isEmpty || currentWord < smallestWord {
                        smallestWord = currentWord
                    }
                }
                currentWord = ""
            } else {
                currentWord.append(char)
            }
        }
        // Check the last segment in the column
        if currentWord.count >= 2 {
            if smallestWord.isEmpty || currentWord < smallestWord {
                smallestWord = currentWord
            }
        }
    }
    
    return smallestWord
}


func testFindSmallestWordInMatrix() {
assert(findSmallestWordInMatrix([["a", "b", "c"], ["#", "d", "#"], ["e", "f", "g"]]) == "abc")
assert(findSmallestWordInMatrix([["a", "#", "c"], ["#", "d", "e"], ["f", "g", "#"]]) == "ce")
assert(findSmallestWordInMatrix([["#", "a", "#"], ["#", "b", "c"], ["#", "#", "d"]]) == "ab")
assert(findSmallestWordInMatrix([["m", "n", "o"], ["#", "#", "#"], ["p", "q", "r"]]) == "mno")
assert(findSmallestWordInMatrix([["x", "y", "#"], ["#", "z", "a"], ["b", "c", "d"]]) == "ad")
}

testFindSmallestWordInMatrix()