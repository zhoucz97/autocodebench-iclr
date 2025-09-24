func determineSign(_ n: Int) -> String {
    if n > 0 {
        return "positive"
    } else if n < 0 {
        return "negative"
    } else {
        return "zero"
    }
}


func testDetermineSign() {
    assert(determineSign(3) == "positive", "3 should be positive.")
    assert(determineSign(0) == "zero", "0 should be zero.")
    assert(determineSign(-5) == "negative", "-5 should be negative.")
    assert(determineSign(1000000000) == "positive", "1000000000 should be positive.")
    assert(determineSign(-1000000000) == "negative", "-1000000000 should be negative.")
    assert(determineSign(1) == "positive", "1 should be positive.")
}

// Call the test function to verify the functionality
testDetermineSign()