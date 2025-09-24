proc find_third_largest {numbers} {
    # Remove duplicates and sort the list in descending order
    set unique_numbers [lsort -unique -integer -decreasing $numbers]
    
    # Determine the length of the unique list
    set len [llength $unique_numbers]
    
    # If there are at least 3 unique numbers, return the third one
    if {$len >= 3} {
        return [lindex $unique_numbers 2]
    } else {
        # Otherwise, return the largest number (first element)
        return [lindex $unique_numbers 0]
    }
}


proc check {find_third_largest} {
if {[find_third_largest {5 4 3 2 1}] == 3} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[find_third_largest {10 10 10 10}] == 10} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[find_third_largest {5 7 3 9 6}] == 6} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[find_third_largest {1 2}] == 2} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[find_third_largest {20 10 20 30 40}] == 20} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[find_third_largest {1}] == 1} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[find_third_largest {-1 -2 -3 -4}] == -2} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check find_third_largest