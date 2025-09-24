func hasCloseElements(_ numbers: [Double], _ threshold: Double) -> Bool {
    guard numbers.count >= 2 else {
        return false
    }
    
    let sortedNumbers = numbers.sorted()
    
    for i in 0..<sortedNumbers.count - 1 {
        let distance = sortedNumbers[i + 1] - sortedNumbers[i]
        if distance < threshold {
            return true
        }
    }
    
    return false
}


func check(_ hasCloseElements: ([Double], Double) -> Bool) {
assert(hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) == true)
assert(hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) == false)
assert(hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) == true)
assert(hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) == false)
assert(hasCloseElements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) == true)
assert(hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) == true)
assert(hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) == false)
}

check(hasCloseElements)