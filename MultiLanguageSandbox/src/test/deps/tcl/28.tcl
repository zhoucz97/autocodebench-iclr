proc calculate_max_necklace_length {totalVolume lossPerBead} {
    set max_length 0
    set optimal_n 0
    set found_unique 0
    
    # Iterate possible n from 1 up to the maximum possible (totalVolume / lossPerBead)
    for {set n 1} {$n * $lossPerBead < $totalVolume} {incr n} {
        set volume_per_bead [expr {($totalVolume - $n * $lossPerBead) / $n}]
        if {$volume_per_bead <= 0} {
            continue
        }
        set diameter [expr {0.3 * sqrt($volume_per_bead)}]
        set current_length [expr {$n * $diameter}]
        
        if {$current_length > $max_length} {
            set max_length $current_length
            set optimal_n $n
            set found_unique 1
        } elseif {$current_length == $max_length && $found_unique} {
            # Multiple optimal solutions found
            return 0
        }
    }
    
    if {$max_length == 0} {
        return 0
    } else {
        return $optimal_n
    }
}


proc check {calculate_max_necklace_length} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [calculate_max_necklace_length 100 10] "5" {Test case 1}
assert_equal [calculate_max_necklace_length 15 4] "2" {Test case 2}
assert_equal [calculate_max_necklace_length 5000 50] "50" {Test case 3}
assert_equal [calculate_max_necklace_length 300 30] "5" {Test case 4}
assert_equal [calculate_max_necklace_length 2000 200] "5" {Test case 5}
}

# Call the check procedure
check calculate_max_necklace_length