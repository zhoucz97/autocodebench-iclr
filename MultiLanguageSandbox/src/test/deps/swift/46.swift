func arithmeticSequenceLastTerm(_ a1: Int, _ a2: Int, _ n: Int) -> Int {
    // Calculate the common difference (d)
    let d = a2 - a1
    
    // Calculate the nth term using the formula: a_n = a1 + (n - 1) * d
    let nthTerm = a1 + (n - 1) * d
    
    return nthTerm
}


func testArithmeticSequenceLastTerm() {
    assert(arithmeticSequenceLastTerm(1, 4, 100) == 298, "The 100th term should be 298.")
    assert(arithmeticSequenceLastTerm(5, 9, 20) == 81, "The 20th term should be 77.")
    assert(arithmeticSequenceLastTerm(-3, 1, 10) == 33, "The 10th term should be 33.")
    assert(arithmeticSequenceLastTerm(0, 2, 50) == 98, "The 50th term should be 98.")
    assert(arithmeticSequenceLastTerm(-100, -97, 1000) == 2897, "The 1000th term should be 2897.")
    assert(arithmeticSequenceLastTerm(2, 5, 3) == 8, "The 3rd term should be 8.")
}

// Call the test function to verify the functionality
testArithmeticSequenceLastTerm()