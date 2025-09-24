func calculateProductOfSumAndValue(_ a: Int, _ b: Int, _ c: Int) -> Int {
    return (a + b) * c
}


func testCalculateProductOfSumAndValue() {
    assert(calculateProductOfSumAndValue(1, 2, 3) == 9, "Test Case 1 Failed")
    assert(calculateProductOfSumAndValue(-1, 2, 3) == 3, "Test Case 2 Failed")
    assert(calculateProductOfSumAndValue(-1, -2, -3) == 9, "Test Case 3 Failed")
    assert(calculateProductOfSumAndValue(0, 0, 1) == 0, "Test Case 4 Failed")
    assert(calculateProductOfSumAndValue(10, 20, 30) == 900, "Test Case 5 Failed")
    assert(calculateProductOfSumAndValue(-5, 5, 0) == 0, "Test Case 6 Failed")
    assert(calculateProductOfSumAndValue(100, 200, -1) == -300, "Test Case 7 Failed")
}

testCalculateProductOfSumAndValue()