func absoluteValue(_ n: Int) -> Int {
    // Check if the absolute value of n exceeds 10000
    if abs(n) > 10000 {
        fatalError("The absolute value of \(n) exceeds the limit of 10000.")
    }
    return abs(n)
}


func testAbsoluteValue() {
    assert(absoluteValue(-3) == 3, "-3's absolute value should be 3.")
    assert(absoluteValue(5) == 5, "5's absolute value should be 5.")
    assert(absoluteValue(0) == 0, "0's absolute value should be 0.")
    assert(absoluteValue(-10000) == 10000, "-10000's absolute value should be 10000.")
    assert(absoluteValue(10000) == 10000, "10000's absolute value should be 10000.")
    assert(absoluteValue(-1) == 1, "-1's absolute value should be 1.")
}

// Call the test function to verify the functionality
testAbsoluteValue()