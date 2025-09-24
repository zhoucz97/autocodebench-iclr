proc max_difference {nums} {
    set max_diff -1
    set min_num [lindex $nums 0]
    
    for {set i 1} {$i < [llength $nums]} {incr i} {
        set current_num [lindex $nums $i]
        if {$current_num > $min_num} {
            set diff [expr {$current_num - $min_num}]
            if {$diff > $max_diff} {
                set max_diff $diff
            }
        } else {
            set min_num $current_num
        }
    }
    
    return $max_diff
}


proc check {max_difference} {
if {[max_difference {7 1 5 4}] == 4} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[max_difference {9 4 3 2}] == -1} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[max_difference {2 3 10 6 4 8 1}] == 8} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[max_difference {7 9 5 6 3 2}] == 2} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[max_difference {1 5 2 10}] == 9} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[max_difference {10 10 10 10}] == -1} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[max_difference {1 2 3 4 5 6 7 8 9 10}] == 9} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check max_difference