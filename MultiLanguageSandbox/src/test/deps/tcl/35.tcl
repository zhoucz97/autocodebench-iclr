proc maxSubmatrixSum {matrix} {
    set rows [llength $matrix]
    if {$rows == 0} {
        return 0
    }
    set cols [llength [lindex $matrix 0]]
    set max_sum -128 ;# Initialize with the smallest possible value (-127)

    for {set top 0} {$top < $rows} {incr top} {
        # Initialize an array to store the sum of columns from top to bottom
        set temp_col_sums [list]
        for {set col 0} {$col < $cols} {incr col} {
            lappend temp_col_sums 0
        }

        for {set bottom $top} {$bottom < $rows} {incr bottom} {
            # Update the column sums by adding the current row's elements
            for {set col 0} {$col < $cols} {incr col} {
                set current_val [lindex $matrix $bottom $col]
                lset temp_col_sums $col [expr {[lindex $temp_col_sums $col] + $current_val}]
            }

            # Apply Kadane's algorithm on the temp_col_sums to find the maximum subarray sum
            set current_max [lindex $temp_col_sums 0]
            set global_max $current_max

            for {set k 1} {$k < $cols} {incr k} {
                set current_max [expr {max([lindex $temp_col_sums $k], $current_max + [lindex $temp_col_sums $k])}]
                if {$current_max > $global_max} {
                    set global_max $current_max
                }
            }

            if {$global_max > $max_sum} {
                set max_sum $global_max
            }
        }
    }

    return $max_sum
}


proc check {maxSubmatrixSum} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [maxSubmatrixSum {{0 -2 -7 0} {9 2 -6 2} {-4 1 -4 1} {-1 8 0 -2}}] "15" "Test case 1"
assert_equal [maxSubmatrixSum {{-1 -1 -1} {-1 9 -1} {-1 -1 -1}}] "9" "Test case 2"
assert_equal [maxSubmatrixSum {{1 2 3} {4 5 6} {7 8 9}}] "45" "Test case 3"
assert_equal [maxSubmatrixSum {{-1 -2 -3} {-4 -5 -6} {-7 -8 -9}}] "-1" "Test case 4"
assert_equal [maxSubmatrixSum {{5 -4 2} {-3 8 -1} {4 -2 6}}] "15" "Test case 5"
}

# Call the check procedure
check maxSubmatrixSum