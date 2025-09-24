proc calculate_nth_term {a1 a2 n} {
    # Calculate the common difference d
    set d [expr {$a2 - $a1}]
    
    # Calculate the nth term using the formula a_n = a1 + (n-1)*d
    set nth_term [expr {$a1 + ($n - 1) * $d}]
    
    return $nth_term
}


proc check_calculate_nth_term {} {
    proc assert_equal {actual expected message} {
        if {$actual != $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [calculate_nth_term 1 4 100] 298 "Test case 1: nth term for 1, 4, 100"
    assert_equal [calculate_nth_term 5 7 10] 23 "Test case 2: nth term for 5, 7, 10"
    assert_equal [calculate_nth_term -1 1 50] 97 "Test case 3: nth term for -1, 1, 50"
    assert_equal [calculate_nth_term 2 5 20] 59 "Test case 4: nth term for 2, 5, 20"
    assert_equal [calculate_nth_term 0 2 100] 198 "Test case 5: nth term for 0, 2, 100"
}

check_calculate_nth_term