proc calculate_absolute_value {n} {
    if {$n < 0} {
        return [expr {-1 * $n}]
    } else {
        return $n
    }
}


proc check_calculate_absolute_value {} {
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [calculate_absolute_value -3] 3 "Test case 1: Absolute value of -3"
    assert_equal [calculate_absolute_value 5] 5 "Test case 2: Absolute value of 5"
    assert_equal [calculate_absolute_value 0] 0 "Test case 3: Absolute value of 0"
    assert_equal [calculate_absolute_value -10000] 10000 "Test case 4: Absolute value of -10000"
    assert_equal [calculate_absolute_value 10000] 10000 "Test case 5: Absolute value of 10000"
}

check_calculate_absolute_value