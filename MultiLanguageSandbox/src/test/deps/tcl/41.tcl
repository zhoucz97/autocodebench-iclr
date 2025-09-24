proc calculate_expression_value {a b c} {
    return [expr {($a + $b) * $c}]
}


proc test_calculate_expression_value {} {
    # Test case utility
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [calculate_expression_value 2 3 5] 25 "Test case 1"
    assert_equal [calculate_expression_value 10 20 3] 90 "Test case 2"
    assert_equal [calculate_expression_value -4 7 2] 6 "Test case 3"
    assert_equal [calculate_expression_value 0 0 0] 0 "Test case 4"
    assert_equal [calculate_expression_value 10000 -9999 2] 2 "Test case 5"
}

# Call the test procedure
test_calculate_expression_value