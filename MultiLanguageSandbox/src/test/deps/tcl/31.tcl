proc calculate_final_position {n p k} {
    set t [expr {(2 * $k * $p) % $n}]
    return $t
}


proc check {calculate_final_position} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [calculate_final_position 12 3 1] 3 "Test case 1"
assert_equal [calculate_final_position 20 7 4] 8 "Test case 2"
assert_equal [calculate_final_position 15 4 7] 13 "Test case 3"
assert_equal [calculate_final_position 100 25 50] 50 "Test case 4"
assert_equal [calculate_final_position 50 20 15] 0 "Test case 5"
assert_equal [calculate_final_position 30 5 17] 25 "Test case 6"
}

# Call the check procedure
check calculate_final_position