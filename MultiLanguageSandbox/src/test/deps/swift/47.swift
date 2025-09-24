func multiply(_ a: Int, _ b: Int) -> Int {
    // Ensure both a and b are within the valid range (1 to 50,000)
    guard a >= 1 && a <= 50_000, b >= 1 && b <= 50_000 else {
        fatalError("Both integers must be between 1 and 50,000")
    }
    
    // Calculate and return the product
    return a * b
}


func testMultiply() {
    assert(multiply(3, 4) == 12, "Product of 3 and 4 should be 12.")
    assert(multiply(10, 5) == 50, "Product of 10 and 5 should be 50.")
    assert(multiply(36, 18) == 648, "Product of 36 and 18 should be 648.")
    assert(multiply(1, 50000) == 50000, "Product of 1 and 50000 should be 50000.")
    assert(multiply(123, 456) == 56088, "Product of 123 and 456 should be 56088.")
    assert(multiply(500, 100) == 50000, "Product of 500 and 100 should be 50000.")
}

// Call the test function to verify the functionality
testMultiply()