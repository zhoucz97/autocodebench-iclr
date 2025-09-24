proc reverse_three_digit_number {n} {
    # Ensure the input is a three-digit number by padding with leading zeros if necessary
    set padded [format "%03d" $n]
    
    # Reverse the string representation of the number
    set reversed [string reverse $padded]
    
    return $reversed
}


proc check_reverse_three_digit_number {} {
    proc assert_equal {actual expected message} {
        if {$actual ne $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [reverse_three_digit_number 358] "853" "Test case 1: Reverse of 358"
    assert_equal [reverse_three_digit_number 100] "001" "Test case 2: Reverse of 100"
    assert_equal [reverse_three_digit_number 20] "020" "Test case 3: Reverse of 020"
    assert_equal [reverse_three_digit_number 678] "876" "Test case 4: Reverse of 678"
    assert_equal [reverse_three_digit_number 1] "100" "Test case 5: Reverse of 001"
}

check_reverse_three_digit_number