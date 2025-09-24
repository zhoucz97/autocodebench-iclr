func convertIntToBoolToInt(_ value: Int) -> Int {
    let boolValue = value != 0
    return boolValue ? 1 : 0
}


func testConvertIntToBoolToInt() {
    assert(convertIntToBoolToInt(5) == 1, "Conversion of 5 should return 1.")
    assert(convertIntToBoolToInt(0) == 0, "Conversion of 0 should return 0.")
    assert(convertIntToBoolToInt(-10) == 1, "Conversion of -10 should return 1.")
    assert(convertIntToBoolToInt(1) == 1, "Conversion of 1 should return 1.")
    assert(convertIntToBoolToInt(-1) == 1, "Conversion of -1 should return 1.")
    assert(convertIntToBoolToInt(100) == 1, "Conversion of 100 should return 1.")
    assert(convertIntToBoolToInt(-100) == 1, "Conversion of -100 should return 1.")
}

// Call the test function to verify the functionality
testConvertIntToBoolToInt()