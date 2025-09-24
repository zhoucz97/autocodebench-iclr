proc multiply {A B} {
    # Ensure A and B are positive integers within the specified range
    if {![string is integer -strict $A] || ![string is integer -strict $B] || $A < 1 || $B < 1 || $A > 50000 || $B > 50000} {
        error "Both A and B must be positive integers between 1 and 50,000."
    }
    
    # Calculate the product
    set product [expr {$A * $B}]
    
    # Return the product
    return $product
}


proc check_multiply {} {
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [multiply 3 4] 12 "Test case 1: Multiplying 3 and 4"
    assert_equal [multiply 36 18] 648 "Test case 2: Multiplying 36 and 18"
    assert_equal [multiply 1 50000] 50000 "Test case 3: Multiplying 1 and 50000"
    assert_equal [multiply 123 456] 56088 "Test case 4: Multiplying 123 and 456"
    assert_equal [multiply 500 100] 50000 "Test case 5: Multiplying 500 and 100"
}

check_multiply