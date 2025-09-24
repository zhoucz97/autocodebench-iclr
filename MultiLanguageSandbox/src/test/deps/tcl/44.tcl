proc int_to_bool_to_int {int_value} {
    # Convert the integer to a boolean (non-zero becomes true, zero becomes false)
    set bool_value [expr {$int_value != 0}]
    
    # Convert the boolean back to an integer (true becomes 1, false becomes 0)
    set result [expr {$bool_value ? 1 : 0}]
    
    return $result
}


proc check_int_to_bool_to_int {} {
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [int_to_bool_to_int 3] 1 "Test case 1: Integer 3 to Boolean and back"
    assert_equal [int_to_bool_to_int 0] 0 "Test case 2: Integer 0 to Boolean and back"
    assert_equal [int_to_bool_to_int -5] 1 "Test case 3: Integer -5 to Boolean and back"
    assert_equal [int_to_bool_to_int 100] 1 "Test case 4: Integer 100 to Boolean and back"
    assert_equal [int_to_bool_to_int -100] 1 "Test case 5: Integer -100 to Boolean and back"
}

check_int_to_bool_to_int