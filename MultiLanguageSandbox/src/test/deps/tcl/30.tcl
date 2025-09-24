proc find_min_unique_sequence_length {mailbox_sequence} {
    set n [string length $mailbox_sequence]
    for {set K 1} {$K <= $n} {incr K} {
        set seen [dict create]
        set unique 1
        for {set i 0} {$i <= $n - $K} {incr i} {
            set substring [string range $mailbox_sequence $i [expr {$i + $K - 1}]]
            if {[dict exists $seen $substring]} {
                set unique 0
                break
            }
            dict set seen $substring 1
        }
        if {$unique} {
            return $K
        }
    }
    return $n ;# in case all sequences are unique only when K is the full length
}


proc check {find_min_unique_sequence_length} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [find_min_unique_sequence_length "AABCAA"] 3 "Test case 1"
assert_equal [find_min_unique_sequence_length "XYZXYZ"] 4 "Test case 2"
assert_equal [find_min_unique_sequence_length "BBBBB"] 5 "Test case 3"
assert_equal [find_min_unique_sequence_length "ABCDEFA"] 2 "Test case 4"
assert_equal [find_min_unique_sequence_length "GHIJKLGH"] 3 "Test case 5"
assert_equal [find_min_unique_sequence_length "RSTUVWXYZRST"] 4 "Test case 6"
}

# Call the check procedure
check find_min_unique_sequence_length