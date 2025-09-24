proc can_be_equal_by_rotation {mat target} {
    # Check if the original matrix matches the target
    if {$mat eq $target} {
        return 1
    }
    
    # Function to rotate the matrix 90 degrees clockwise
    proc rotate_90 {matrix} {
        set n [llength $matrix]
        set rotated {}
        for {set i 0} {$i < $n} {incr i} {
            set new_row {}
            for {set j [expr {$n - 1}]} {$j >= 0} {incr j -1} {
                lappend new_row [lindex $matrix $j $i]
            }
            lappend rotated $new_row
        }
        return $rotated
    }
    
    # Rotate up to 3 times (total 270 degrees) and check each time
    set current_mat $mat
    for {set i 1} {$i <= 3} {incr i} {
        set current_mat [rotate_90 $current_mat]
        if {$current_mat eq $target} {
            return 1
        }
    }
    
    return 0
}


proc check {can_be_equal_by_rotation} {
if {[can_be_equal_by_rotation {{1 0} {0 1}} {{0 1} {1 0}}] == 1} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[can_be_equal_by_rotation {{0 1} {1 1}} {{1 0} {0 1}}] == 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[can_be_equal_by_rotation {{1 1 0} {0 0 1} {0 1 0}} {{0 0 1} {1 1 0} {0 1 0}}] == 0} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[can_be_equal_by_rotation {{1 1} {1 0}} {{0 1} {1 1}}] == 1} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[can_be_equal_by_rotation {{0 0} {0 0}} {{0 0} {0 0}}] == 1} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[can_be_equal_by_rotation {{1 0 0} {0 1 0} {0 0 1}} {{0 0 1} {0 1 0} {1 0 0}}] == 1} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[can_be_equal_by_rotation {{1 0 1} {0 1 0} {1 0 1}} {{1 0 1} {0 1 0} {1 0 1}}] == 1} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check can_be_equal_by_rotation