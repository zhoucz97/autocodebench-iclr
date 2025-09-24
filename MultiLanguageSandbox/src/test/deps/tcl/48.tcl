proc power_of_two {n} {
    return [expr {1 << $n}]
}


proc check_power_of_two {} {
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [power_of_two 3] 8 "Test case 1: 2^3"
    assert_equal [power_of_two 10] 1024 "Test case 2: 2^10"
    assert_equal [power_of_two 0] 1 "Test case 3: 2^0"
    assert_equal [power_of_two 5] 32 "Test case 4: 2^5"
    assert_equal [power_of_two 15] 32768 "Test case 5: 2^15"
}

check_power_of_two