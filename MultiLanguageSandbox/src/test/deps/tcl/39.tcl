proc maximize_value {n m item_list} {
    # Initialize a DP table with (n+1) rows and (m+1) columns
    # dp[i][j] represents the maximum value achievable with first i items and budget j
    set dp [lrepeat [expr {$n + 1}] [lrepeat [expr {$m + 1}] 0]]
    
    # Fill the DP table
    for {set i 1} {$i <= $m} {incr i} {
        lassign [lindex $item_list [expr {$i - 1}]] price importance
        for {set j 0} {$j <= $n} {incr j} {
            if {$j >= $price} {
                # Option 1: Don't take the current item
                set option1 [lindex $dp $j [expr {$i - 1}]]
                # Option 2: Take the current item
                set option2 [expr {[lindex $dp [expr {$j - $price}] [expr {$i - 1}]] + $price * $importance}]
                # Choose the maximum of the two options
                lset dp $j $i [expr {max($option1, $option2)}]
            } else {
                # Can't take the current item (not enough budget)
                lset dp $j $i [lindex $dp $j [expr {$i - 1}]]
            }
        }
    }
    
    # The answer is in the bottom-right corner of the DP table
    return [lindex $dp $n $m]
}


proc check_maximize_value {} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [maximize_value 1000 5 {{800 2} {400 5} {300 5} {400 3} {200 2}}] 3900 "Test case 1"
assert_equal [maximize_value 500 3 {{300 4} {200 3} {100 2}}] 1800 "Test case 2"
assert_equal [maximize_value 200 2 {{100 1} {50 2}}] 200 "Test case 3"
assert_equal [maximize_value 700 4 {{350 3} {200 2} {100 5} {50 2}}] 2050 "Test case 4"
assert_equal [maximize_value 1500 6 {{500 5} {400 4} {300 3} {200 2} {100 1} {50 1}}] 5500 "Test case 5"
}

# Call the check procedure
check_maximize_value