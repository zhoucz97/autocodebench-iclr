proc calculate_consistent_pairs {k rankings} {
    set n [llength [lindex $rankings 0]]
    set consistent_pairs 0
    
    # Iterate over all possible pairs of cows
    for {set i 0} {$i < $n} {incr i} {
        for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
            set consistent 1
            # Determine the initial relationship between i and j in the first session
            set first_session [lindex $rankings 0]
            set pos_i [lsearch -exact $first_session [expr {$i + 1}]]
            set pos_j [lsearch -exact $first_session [expr {$j + 1}]]
            
            if {$pos_i < $pos_j} {
                # i is ranked higher than j in the first session; check all other sessions
                for {set session 1} {$session < $k} {incr session} {
                    set current_session [lindex $rankings $session]
                    set current_pos_i [lsearch -exact $current_session [expr {$i + 1}]]
                    set current_pos_j [lsearch -exact $current_session [expr {$j + 1}]]
                    if {$current_pos_i >= $current_pos_j} {
                        set consistent 0
                        break
                    }
                }
            } else {
                # j is ranked higher than i in the first session; check all other sessions
                for {set session 1} {$session < $k} {incr session} {
                    set current_session [lindex $rankings $session]
                    set current_pos_i [lsearch -exact $current_session [expr {$i + 1}]]
                    set current_pos_j [lsearch -exact $current_session [expr {$j + 1}]]
                    if {$current_pos_j >= $current_pos_i} {
                        set consistent 0
                        break
                    }
                }
            }
            if {$consistent} {
                incr consistent_pairs
            }
        }
    }
    return $consistent_pairs
}


proc check {calculate_consistent_pairs} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [calculate_consistent_pairs 2 {{1 2 3} {2 1 3}}] 2 "Test case 1"
assert_equal [calculate_consistent_pairs 3 {{3 2 1} {1 3 2} {2 1 3}}] 0 "Test case 2"
assert_equal [calculate_consistent_pairs 3 {{1 2 3 4} {2 3 4 1} {3 4 1 2}}] 1 "Test case 3"
assert_equal [calculate_consistent_pairs 4 {{1 2 3 4} {4 3 2 1} {1 4 2 3} {3 1 4 2}}] 0 "Test case 4"
}

# Call the check procedure
check calculate_consistent_pairs