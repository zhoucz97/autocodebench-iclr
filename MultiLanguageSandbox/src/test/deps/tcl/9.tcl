proc count_pairs_with_diff_k {nums k} {
    set count 0
    set n [llength $nums]
    
    for {set i 0} {$i < $n} {incr i} {
        for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
            set diff [expr {abs([lindex $nums $i] - [lindex $nums $j])}]
            if {$diff == $k} {
                incr count
            }
        }
    }
    
    return $count
}


proc check {count_pairs_with_diff_k} {
if {[count_pairs_with_diff_k {1 2 3 4} 1] == 3} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[count_pairs_with_diff_k {1 3 5 7} 2] == 3} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[count_pairs_with_diff_k {1 1 1 1} 0] == 6} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[count_pairs_with_diff_k {1 5 9 1 5 9} 4] == 8} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[count_pairs_with_diff_k {4 7 10 13 16} 3] == 4} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[count_pairs_with_diff_k {-2 2 4 6} 4] == 2} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[count_pairs_with_diff_k {0 0 0 0} 1] == 0} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check count_pairs_with_diff_k