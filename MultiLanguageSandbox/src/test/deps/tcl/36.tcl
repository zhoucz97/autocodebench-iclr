proc maximize_herb_value {totalTime numHerbs herbData} {
    # Initialize a DP array where dp[j] represents the maximum value achievable with time j
    set dp [lrepeat [expr {$totalTime + 1}] 0]
    
    foreach herb $herbData {
        lassign $herb time value
        # Iterate from totalTime down to time to avoid reusing the same herb multiple times
        for {set j $totalTime} {$j >= $time} {incr j -1} {
            if {[lindex $dp [expr {$j - $time}]] + $value > [lindex $dp $j]} {
                lset dp $j [expr {[lindex $dp [expr {$j - $time}]] + $value}]
            }
        }
    }
    
    return [lindex $dp $totalTime]
}


proc check {maximize_herb_value} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [maximize_herb_value 60 3 {{10 10} {20 40} {30 50}}] 100 "Test case 1"
assert_equal [maximize_herb_value 50 4 {{5 10} {10 20} {20 30} {25 40}}] 80 "Test case 2"
assert_equal [maximize_herb_value 30 2 {{15 10} {15 20}}] 30 "Test case 3"
assert_equal [maximize_herb_value 100 5 {{10 10} {20 20} {30 30} {40 40} {50 50}}] 100 "Test case 4"
assert_equal [maximize_herb_value 75 3 {{25 35} {30 40} {20 25}}] 100 "Test case 5"
assert_equal [maximize_herb_value 80 4 {{10 15} {20 25} {30 35} {40 45}}] 95 "Test case 6"
}

# Call the check procedure
check maximize_herb_value