proc determine_sign {N} {
    if {$N > 0} {
        return "positive"
    } elseif {$N == 0} {
        return "zero"
    } else {
        return "negative"
    }
}


proc check_determine_sign {} {
    proc assert_equal {actual expected message} {
        if {$actual ne $expected} {
            error "$message. Expected $expected but got $actual"
        } else {
            puts "$message - Passed"
        }
    }

    # Test cases
    assert_equal [determine_sign 95] "positive" "Test case 1: Determining sign for 95"
    assert_equal [determine_sign 0] "zero" "Test case 2: Determining sign for 0"
    assert_equal [determine_sign -10] "negative" "Test case 3: Determining sign for -10"
    assert_equal [determine_sign 1] "positive" "Test case 4: Determining sign for 1"
    assert_equal [determine_sign -1] "negative" "Test case 5: Determining sign for -1"
}

check_determine_sign