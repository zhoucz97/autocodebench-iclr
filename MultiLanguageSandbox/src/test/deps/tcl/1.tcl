proc has_close_elements {numbers threshold} {
    # Sort the list of numbers to easily check adjacent elements
    set sorted_numbers [lsort -real $numbers]
    
    # Iterate through the sorted list and check adjacent elements
    for {set i 0} {$i < [llength $sorted_numbers] - 1} {incr i} {
        set current [lindex $sorted_numbers $i]
        set next [lindex $sorted_numbers [expr {$i + 1}]]
        
        # Calculate the absolute difference between adjacent elements
        set diff [expr {abs($current - $next)}]
        
        # If the difference is less than the threshold, return true
        if {$diff < $threshold} {
            return true
        }
    }
    
    # If no such pair is found, return false
    return false
}


# Test cases
proc check {has_close_elements} {
if {[has_close_elements {1.0 2.0 3.9 4.0 5.0 2.2} 0.3] == 1} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[has_close_elements {1.0 2.0 3.9 4.0 5.0 2.2} 0.05] == 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[has_close_elements {1.0 2.0 5.9 4.0 5.0} 0.95] == 1} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[has_close_elements {1.0 2.0 5.9 4.0 5.0} 0.8] == 0} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[has_close_elements {1.0 2.0 3.0 4.0 5.0 2.0} 0.1] == 1} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[has_close_elements {1.1 2.2 3.1 4.1 5.1} 1.0] == 1} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[has_close_elements {1.1 2.2 3.1 4.1 5.1} 0.5] == 0} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check has_close_elements