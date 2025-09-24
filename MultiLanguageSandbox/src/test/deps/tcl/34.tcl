proc generate_bitonic_sequence {numbers} {
    # Sort the numbers in ascending order
    set sorted [lsort -integer $numbers]
    
    # Initialize an empty list for the result
    set result {}
    
    # Get the length of the sorted list
    set len [llength $sorted]
    
    # Use two pointers, one at the start (smallest) and one at the end (largest)
    set left 0
    set right [expr {$len - 1}]
    
    # Alternate between taking from the end and the start
    while {$left <= $right} {
        if {$left < $right} {
            # Take the largest remaining number
            lappend result [lindex $sorted $right]
            # Take the smallest remaining number
            lappend result [lindex $sorted $left]
        } else {
            # If there's one element left, just take it
            lappend result [lindex $sorted $left]
        }
        incr left
        incr right -1
    }
    
    return $result
}


proc check {generate_bitonic_sequence} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [generate_bitonic_sequence {10 -1 3 3 -9}] {10 -9 3 -1 3} "Test case 1"
assert_equal [generate_bitonic_sequence {5 1 9 2 8 3}] {9 1 8 2 5 3} "Test case 2"
assert_equal [generate_bitonic_sequence {-5 -1 -9 -2 -8 -3}] {-1 -9 -2 -8 -3 -5} "Test case 3"
assert_equal [generate_bitonic_sequence {0 0 0 0}] {0 0 0 0} "Test case 4"
assert_equal [generate_bitonic_sequence {1000 500 100 -500}] {1000 -500 500 100} "Test case 5"
}

# Call the check procedure
check generate_bitonic_sequence