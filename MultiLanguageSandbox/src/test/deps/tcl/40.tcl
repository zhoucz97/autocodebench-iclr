proc max_subsequence_sum {sequence} {
    if {[llength $sequence] == 0} {
        return 0
    }
    
    set max_current [lindex $sequence 0]
    set max_global [lindex $sequence 0]
    
    for {set i 1} {$i < [llength $sequence]} {incr i} {
        set current_element [lindex $sequence $i]
        set max_current [expr {max($current_element, $max_current + $current_element)}]
        set max_global [expr {max($max_global, $max_current)}]
    }
    
    return $max_global
}


proc check {max_subsequence_sum} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [max_subsequence_sum {2 -1 3 4 -2}] "8" "Test case 1"
assert_equal [max_subsequence_sum {-3 -2 -1 -4}] "-1" "Test case 2"
assert_equal [max_subsequence_sum {1 -2 3 10 -4 7 -2 5}] "19" "Test case 3"
assert_equal [max_subsequence_sum {-1 -2 -3 -4}] "-1" "Test case 4"
assert_equal [max_subsequence_sum {1 2 3 4 5 -10 5 4 3 2 1}] "20" "Test case 5"
}

# Call the check procedure
check max_subsequence_sum