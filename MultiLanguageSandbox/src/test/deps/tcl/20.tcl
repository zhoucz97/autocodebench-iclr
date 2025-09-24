proc max_sum_subsequence {nums k} {
    # Sort the numbers in descending order
    set sorted_nums [lsort -integer -decreasing $nums]
    # Take the first k elements
    set result [lrange $sorted_nums 0 [expr {$k - 1}]]
    return $result
}


proc check {max_sum_subsequence} {
if {[max_sum_subsequence {1 2 3 4 5} 3] eq {5 4 3}} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[max_sum_subsequence {5 4 3 2 1} 2] eq {5 4}} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[max_sum_subsequence {10 20 30 40 50} 4] eq {50 40 30 20}} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[max_sum_subsequence {1 1 1 1 1} 5] eq {1 1 1 1 1}} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[max_sum_subsequence {-1 -2 -3 -4 -5} 3] eq {-1 -2 -3}} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[max_sum_subsequence {5 1 3 4 2} 2] eq {5 4}} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[max_sum_subsequence {9 8 7 6 5 4 3 2 1 0} 1] eq {9}} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check max_sum_subsequence