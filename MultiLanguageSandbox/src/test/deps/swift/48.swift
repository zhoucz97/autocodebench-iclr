func powerOfTwo(_ n: Int) -> Int {
    return 1 << n
}


func testPowerOfTwo() {
    assert(powerOfTwo(3) == 8, "2^3 should be 8.")
    assert(powerOfTwo(5) == 32, "2^5 should be 32.")
    assert(powerOfTwo(10) == 1024, "2^10 should be 1024.")
    assert(powerOfTwo(0) == 1, "2^0 should be 1.")
    assert(powerOfTwo(15) == 32768, "2^15 should be 32768.")
    assert(powerOfTwo(30) == 1073741824, "2^30 should be 1073741824.")
}

// Call the test function to verify the functionality
testPowerOfTwo()