proc calculate_peaches {n} {
    set peaches 1
    for {set i 1} {$i <= $n} {incr i} {
        set peaches [expr {2 * ($peaches + 1)}]
    }
    return $peaches
}


proc check {calculate_peaches} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [calculate_peaches 3] "10" "Test case 1"
assert_equal [calculate_peaches 5] "46" "Test case 2"
assert_equal [calculate_peaches 6] "94" "Test case 3"
assert_equal [calculate_peaches 7] "190" "Test case 4"
assert_equal [calculate_peaches 8] "382" "Test case 5"
assert_equal [calculate_peaches 2] "4" "Test case 6"
}

# Call the check procedure
check calculate_peaches