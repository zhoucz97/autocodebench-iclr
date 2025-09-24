func reverseThreeDigitNumber(_ number: Int) -> Int {
    // Check if the number is a three-digit number
    if number < 100 || number > 999 {
        return -1
    }
    
    // Extract each digit
    let hundreds = number / 100
    let tens = (number / 10) % 10
    let units = number % 10
    
    // Reverse the digits
    let reversedNumber = units * 100 + tens * 10 + hundreds
    
    return reversedNumber
}


func testReverseThreeDigitNumber() {
    assert(reverseThreeDigitNumber(123) == 321, "Reverse of 123 should be 321.")
    assert(reverseThreeDigitNumber(456) == 654, "Reverse of 456 should be 654.")
    assert(reverseThreeDigitNumber(789) == 987, "Reverse of 789 should be 987.")
    assert(reverseThreeDigitNumber(100) == 1, "Reverse of 100 should be 1.")
    assert(reverseThreeDigitNumber(905) == 509, "Reverse of 905 should be 509.")
    assert(reverseThreeDigitNumber(999) == 999, "Reverse of 999 should be 999.")
    assert(reverseThreeDigitNumber(210) == 12, "Reverse of 210 should be 12.")
}

// Call the test function to verify the functionality
testReverseThreeDigitNumber()